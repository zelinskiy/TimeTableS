{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Text.Encoding as ENC
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import Control.Monad.IO.Class
import Data.List.Split as Split
import Data.List as List
import System.Environment
import Data.Maybe
import Text.Read
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Char

import TimeTableS.Types
import TimeTableS.Settings
import TimeTableS.Utils ((<$$>), orderBy)

{-
TODO:
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

getTimetableRoot :: Int -> IO (Maybe TimeTable)
getTimetableRoot id = getJSON ("http://cist.nure.ua/ias/app/tt/get_schedule?group_id=" ++ show id)

getGroupsRoot :: Int -> IO (Maybe GroupsRoot)
getGroupsRoot fid = getJSON ("http://cist.nure.ua/ias/app/tt/get_groups?faculty_id=" ++ show fid)


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
  where realIds = map fst rows
        data_ = map snd rows
        withSimpleIds = zipWith (\re si -> (re, si)) realIds [1..]
        res = zipWith (,) withSimpleIds data_

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
        Just x -> x
        Nothing -> error "Unable to parse fid"

  unless (fid `elem` map faculty_id facs) $ error "Unable to find fac"
  return fid



selectGroupIdOnFacIO :: Int -> IO Int
selectGroupIdOnFacIO fid = do
  gs' <- groups <$$> getGroupsRoot fid
  let gs = case gs' of
        Nothing -> error "Unable to load from server"
        Just xs -> (orderBy fst . map pair) xs
            where pair g = (group_id g, group_name g)

  let _withShallowIds = withShallowIds gs
  let prettyPrint ((_,si),name) = "("++show si++")"++name++"\n"
  let prettyPrinted = concat $ map prettyPrint _withShallowIds
  let simplifiedGids = map fst _withShallowIds

  putStrLn prettyPrinted
  selected_sgid <- getLine
  let sgid = case readMaybe selected_sgid :: Maybe Int of
        Just x -> x
        Nothing -> error "Unable to parse sgid"

  let gid = case find (\(re, si) -> si == sgid) simplifiedGids of
        Just (re, si) -> re
        Nothing -> error $ "Cant find group " ++ show sgid
  return gid

selectGroupIO :: IO ()
selectGroupIO = do
  fid <- selectFacId
  gid <- selectGroupIdOnFacIO fid
  putStrLn $ "Selected group " ++ show gid
  saveSettings "settings.txt" [("group_id", show gid)]

{-
data Command =
    ShowAllFacs
  | ShowGroupsOnFac Int
  | SelectGroup Int
  | ShowSelectedGroup
    deriving (Eq)
-}

parseInt :: String -> Maybe Int
parseInt n
    | all isDigit n = Just $ read n
    | otherwise     = Nothing

parseCommand :: String -> Maybe Command
parseCommand cmd = case cmd of
  "facs" -> Just ShowAllFacs
  'f':'a':'c':' ':n -> Just . ShowGroupsOnFac =<< parseInt n
  _ -> Nothing

executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  ShowAllFacs -> loadFaculties >>= showFacs

main :: IO ()
main = do
  executeCommand ShowAllFacs
