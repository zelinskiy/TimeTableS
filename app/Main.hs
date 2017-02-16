{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad
import Data.List as List
import Text.Read
import qualified Data.Time.Calendar as C

import TimeTableS.Types
import TimeTableS.Settings
import TimeTableS.Utils
import TimeTableS.Date

{-
TODO:

Shall we trust our json (e.g. in getLessonDates)?
    maybe check thar (_date_start & _date_end) OR _dates?

Save groupId in settings
Show timetable for saved group
Storing timetables locally
Show

* Lenses

-}

data Command =
    ShowAllFacs
  | ShowGroupsOnFac Int
  | SelectGroup Int
  | ShowSelectedGroup
    deriving (Eq)

--------------------------------------------------------------------------------

getJSON :: FromJSON a => String -> IO (Maybe a)
getJSON url = decode <$>simpleHttp url

getFacultiesRoot :: IO (Maybe FacultiesRoot)
getFacultiesRoot = getJSON "http://cist.nure.ua/ias/app/tt/get_faculties"

getTimetableRoot :: String -> IO (Maybe TimeTable)
getTimetableRoot gid =
  getJSON ("http://cist.nure.ua/ias/app/tt/get_schedule?group_id=" ++ gid)

getGroupsRoot :: Int -> IO (Maybe GroupsRoot)
getGroupsRoot fid =
  getJSON ("http://cist.nure.ua/ias/app/tt/get_groups?faculty_id=" ++ show fid)

getGroupsInFaculty :: Int -> IO (Maybe [Group])
getGroupsInFaculty fid = inner <$$> getGroupsRoot fid
  where inner = groups

allFacultiesIds :: IO (Maybe [Int])
allFacultiesIds = inner <$$> getFacultiesRoot
  where inner = (map faculty_id) . faculties

allGroups' :: [Int] -> IO (Maybe [GroupsRoot])
allGroups' ids= fmap sequence . sequence $ map getGroupsRoot ids

--------------------------------------------------------------------------------

withShallowIds :: [(Int, a)] -> [((Int, Int), a)]
withShallowIds rows = res
  where realIds       = map fst rows
        data_         = map snd rows
        withSimpleIds = zipWith (\re si -> (re, si)) realIds [1..]
        res           = zipWith (,) withSimpleIds data_

idWithName :: [(Int, String)] -> String
idWithName rows = concat $ map compact rows
  where compact (index, name) = "(" ++ show index ++ ") " ++name ++ "\n"


loadFaculties :: IO [Faculty]
loadFaculties = do
  facs <- faculties <$$> getFacultiesRoot
  case facs of
        Nothing -> error "Unable to load from server"
        Just fs -> return fs

showFacs :: [Faculty] -> IO ()
showFacs facs = do
  let idNamePairs = map (\f -> (faculty_id f, faculty_name f)) facs
  putStrLn "(id) Name"
  putStrLn $ idWithName idNamePairs
  putStrLn "enter selected_fid"

selectFacId :: IO Int
selectFacId = do
  facs <- loadFaculties
  showFacs facs
  fid' <- getLine
  let fid = case readMaybe fid' :: Maybe Int of
        Just x  -> x
        Nothing -> error "Unable to parse fid"

  unless (fid `elem` map faculty_id facs) $ error "Unable to find fac"
  return fid

--------------------------------------------------------------------------------
loadGroups :: Int -> IO [Group]
loadGroups fid = do
  gs <- groups <$$> getGroupsRoot fid
  case gs of
        Nothing   -> error "Unable to load from server"
        Just res  -> return res

showGroups :: [Group] -> IO ()
showGroups groups = putStrLn $ concat $ pp <$> groups
  where pp g = "("++show (group_id g)++")"++(group_name g)++"\n"

showGroupsWithIds :: [(Int, Group)] -> IO ()
showGroupsWithIds groups = putStrLn $ concat $ pp <$> groups
  where pp (i, g) = "("++show i ++")"++(group_name g)++"\n"

selectGroupIdOnFacIO :: Int -> IO Int
selectGroupIdOnFacIO fid = do
  gs <- orderBy group_name <$> loadGroups fid

  let pairs = (\g -> (group_id g, group_name g)) <$> gs
  let _withShallowIds = withShallowIds pairs
  let simplifiedGids = map fst _withShallowIds
  showGroupsWithIds $ zipWith (,) (snd <$> simplifiedGids) gs

  gid1 <- getLine

  let gid2 = case readMaybe gid1 :: Maybe Int of
        Just x -> x
        Nothing -> error $ "Unable to parse sgid " ++ gid1

  let gid3 = case find (\(re, si) -> si == gid2) simplifiedGids of
        Just (re, si) -> re
        Nothing -> error $ "Cant find group " ++ show gid2
  return gid3

selectGroupIO :: IO ()
selectGroupIO = do
  fid <- selectFacId
  gid <- selectGroupIdOnFacIO fid
  putStrLn $ "Selected group " ++ show gid
  saveSettings "settings.txt" [("group_id", show gid)]

--------------------------------------------------------------------------------

parseCommand :: String -> Maybe Command
parseCommand cmd = case cmd of
  "facs"            -> Just ShowAllFacs
  'f':'a':'c':' ':n -> Just . ShowGroupsOnFac =<< parseInt n
  _                 -> Nothing

executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  ShowAllFacs -> loadFaculties >>= showFacs

storedGroupId :: IO (Maybe String)
storedGroupId = findSetting "group_id" <$> loadSettings "settings.txt"

--------------------------------------------------------------------------------

loadDays :: String -> IO [Day]
loadDays gid = do
  tt <- getTimetableRoot gid
  case tt of
    Just x  -> return $ daysTT x
    Nothing -> error $ "cannot load timetable for gid " ++ gid

allSubjects :: String -> IO [String]
allSubjects gid = do
  days <- loadDays gid
  return $
  --concat $
  --flip (++) "\n" <$$>
    sort $
    nub $
    concat $
    _subject <$$>
    lessons <$>
    days


--TODO: frequency = 2 to 4 per month, not only every week
getLessonDates :: Lesson -> [C.Day]
getLessonDates lesson = res where
  freq = 7
  subj = _subject lesson
  res =
    case ((,)
      <$> (fromTimeTableFormat <$> _date_start lesson)
      <*> (fromTimeTableFormat <$> _date_end   lesson)
      ,   (fromTimeTableFormat <$$> _dates     lesson)) of
        (Just (begin, end), Nothing) -> [begin, (C.addDays freq begin) .. end]
        (Nothing, Just dates) -> dates
        (Nothing, Nothing) ->
          error $ "Lesson without both dates and date_start: " ++ subj



getLessonsOnDate :: String -> C.Day -> IO [Lesson]
getLessonsOnDate gid date = do
  allLessons <- concat <$> lessons <$$> loadDays gid
  return
    $ map     fst
    $ filter  (\(_, ds) -> date `elem` ds)
    $ map     (\l       -> (l, getLessonDates l))
    $ allLessons

--------------------------------------------------------------------------------



main :: IO ()
main = do
  todayDate <- getCurrentDay
  todayLessons <- getLessonsOnDate "5259356" (C.addDays 1 todayDate)
  putStrLn $ concat
    $ map (\l -> _subject l ++ ":" ++ _time_start l ++ "\n")
    $ todayLessons
  putStrLn $ "kek"
