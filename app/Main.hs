module Main where

import Control.Monad
import Data.List as List
import Data.Time.Calendar
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Tuple
import System.Environment

import TimeTableS.Types
import TimeTableS.Settings
import TimeTableS.Utils
import TimeTableS.Date
import TimeTableS.LoadFromServer
import TimeTableS.SelectGroup


{-
TODO:

Shall we trust our json (e.g. in getLessonDates)?
    maybe check thar (_date_start & _date_end) OR _dates?

Save groupId in settings
Show timetable for saved group
Storing timetables locally
Show
IO + Maybe = MaybeT
* Lenses
* remove all these freaking error calls

(!) please make $ stack build --pedantic before commit

-}

--------------------------------------------------------------------------------


--TODO: frequency = 2 to 4 per month, not only every week
getLessonDates :: Lesson -> [Day]
getLessonDates lesson = res where
  freq = 7
  subj = _subject lesson
  res =
    case ((,)
      <$> (fromTimeTableFormat <$> _date_start lesson)
      <*> (fromTimeTableFormat <$> _date_end   lesson)
      ,   (fromTimeTableFormat <$$> _dates     lesson)) of
        (Just (begin, end), Nothing)  -> [begin, (addDays freq begin) .. end]
        (Nothing, Just dates)         -> dates
        (Nothing, Nothing) ->
          error $ "Lesson without both dates and date_start: " ++ subj
        (_, _) -> error "wtf"



getLessonsOnDate :: String -> Day -> IO [Lesson]
getLessonsOnDate gid date = (snd . head) <$> getLessonsOnDates gid [date]

getLessonsOnDates :: String -> [Day] -> IO [(Day, [Lesson])]
getLessonsOnDates gid dates = do
  allLessons <- concat <$> lessons <$$> loadDays gid
  return
    $ groupTuples fst
    $ map swap
    $ concat
    $ map (filter (\(_, d) -> elem d dates))
    $ map (\l -> [(l,d) | d <- getLessonDates l])
    $ allLessons

prettyPrintLesson :: Lesson -> String
prettyPrintLesson l =
  let margin = replicate 12 ' ' in
  "(" ++ showShortTypeLesson (_type l) ++ ")["
      ++ _time_start l ++ "]"
      ++ _subject l ++ "\n" ++ margin ++"|"
      ++ intercalate
            ("\n" ++ margin ++"|")
            (teacher_name <$> _teachers l)
      ++ "\n" ++ margin ++ "[["
      ++ (concat $ auditory_name <$> _auditories l)
      ++ "]]\n"

prettyPrintLessonsWithDay :: (Day, [Lesson]) -> String
prettyPrintLessonsWithDay (d, ls) = "\n{# " ++ show d ++ " #}\n\n"
  ++ concatMap prettyPrintLesson ls

showLessonsWithDays :: [(Day, [Lesson])] -> IO ()
showLessonsWithDays = putStrLn . concat . map prettyPrintLessonsWithDay

showLessons :: [Lesson] -> IO ()
showLessons = putStrLn . concat . map prettyPrintLesson

--------------------------------------------------------------------------------

--TODO: split in multiple instances (GADTs?)
data Command =
    SelectGroup
  | SelectedGroup
  | Today
  | Tomorrow
  | OnDay         Day
  | SinceToday    Int
  | SinceDay      Day Int
  | Help
    deriving (Show, Eq)

data Arg =
    IArg Int
  | SArg String
  | DArg Day
    deriving (Show, Eq)

parseArg :: String -> Maybe Arg
parseArg s =
  let splitted  = sequence $  parseInt <$> splitOn "." s
  in case length  <$> splitted of
    Just 3  -> Just  $ DArg $ fromTimeTableFormat s
    Just 1  -> IArg <$> parseInt s
    Nothing -> Just $ SArg s
    _       -> error "unknown argument format"


preprocessCommand :: String -> (String, [Arg])
preprocessCommand s = (cmd, args) where
  splitted  = splitOn " " s
  cmd       = head splitted
  args      = catMaybes $ parseArg <$> tail splitted

parseCommand :: String -> Maybe Command
parseCommand cmd = case preprocessCommand cmd of
  ("group", [])                       -> Just $ SelectedGroup
  ("setgroup", [])                    -> Just $ SelectGroup
  ("wassup", [DArg date, IArg days])  -> Just $ SinceDay date days
  ("wassup", [IArg days, DArg date])  -> Just $ SinceDay date days
  ("wassup", [DArg date])             -> Just $ OnDay date
  ("wassup", [IArg days])             -> Just $ SinceToday days
  ("wassup", [])                      -> Just $ Today
  ("wassup", [SArg "tomorrow"])       -> Just $ Tomorrow
  ("help", [])                        -> Just $ Help
  _                                   -> Nothing

executeCommand :: Command -> IO ()
executeCommand cmd = do
  today <- getCurrentDay
  --very unsafe
  gid <- fromJust <$> storedGroupId
  case cmd of
    SinceDay date days  ->  do
                            let till = addDays (toInteger days - 1) date
                            putStrLn $ "Lessons from "  ++ show date
                                  ++ " till " ++ show till ++ " ..."
                            getLessonsOnDates gid [today .. till]
                            >>= showLessonsWithDays
    OnDay date          ->  putStrLn ("Lessons for " ++ show date)
                            >> getLessonsOnDate gid date
                            >>= showLessons
    SinceToday days     ->  do
                            let till = addDays (toInteger days - 1) today
                            putStrLn $ "Lessons till " ++ show till
                            getLessonsOnDates gid [today .. till]
                            >>= showLessonsWithDays
    Today               ->  putStrLn "Lessons for today..."
                            >> getLessonsOnDate gid today
                            >>= showLessons
    Tomorrow            ->  putStrLn "Lessons for tomorrow..."
                            >>  getLessonsOnDate gid (addDays 1 today)
                            >>= showLessons
    SelectGroup         ->  selectGroupIO
    SelectedGroup       ->  storedGroupId >>= print
    Help                ->  putStrLn $ "\n[Commands]\n\n"
          ++ "wassup                lessons for today\n"
          ++ "wassup 10             lessons for next 10 days\n"
          ++ "wassup 01.01.2017 10  lessons for next 10 days since 01.01.2017\n"
          ++ "wassup 01.01.2017     lessons on specified date\n"
          ++ "group                 show me current group\n"
          ++ "setgroup              run interactive group selection\n"
          ++ "help                  show this help\n"
          ++ "*note                 arguments order is irrelenant (probably)\n"

storedGroupId :: IO (Maybe String)
storedGroupId = findSetting "group_id" <$> loadSettings "settings.txt"

--------------------------------------------------------------------------------

run :: String -> IO ()
run cmd = case parseCommand cmd of
  Just c -> executeCommand c
  Nothing -> putStrLn "error"

main :: IO ()
main = do
  gid <- storedGroupId
  unless (isJust gid) $
    putStrLn "(!!) First you need to select your group:\n"
    >> selectGroupIO

  maybeCmd <- parseCommand <$> concat <$> getArgs
  if not (isJust maybeCmd)
  then do
    putStrLn "Unable to parse command"
  else do
    let cmd = fromJust maybeCmd
    executeCommand cmd
    putStrLn "done"
