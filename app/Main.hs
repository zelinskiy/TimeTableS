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

--import Lib
import TimeTableS.Types

{-
TODO:
Lenses
-}

prefix_allfacs = "http://cist.nure.ua/ias/app/tt/get_faculties"
prefix_fac = "http://cist.nure.ua/ias/app/tt/get_groups?faculty_id="
prefix_timetable = "http://cist.nure.ua/ias/app/tt/get_schedule?group_id="

fmap2 f = fmap (fmap f)
(<$$>) f x = f `fmap2` x
infixl 4 <$$>

getJSON :: String -> IO B.ByteString
getJSON url = simpleHttp url

getFacultiesRoot :: IO (Maybe FacultiesRoot)
getFacultiesRoot = decode <$> getJSON prefix_allfacs

getTimetableRoot :: Int -> IO (Maybe TimeTable)
getTimetableRoot id = decode <$> getJSON (prefix_timetable ++ show id)

getGroupsRoot :: Int -> IO (Maybe GroupsRoot)
getGroupsRoot fid = decode <$> getJSON (prefix_fac ++ show fid)



getGroupsInFaculty' :: GroupsRoot -> [Group]
getGroupsInFaculty' root = groups root

getGroupsInFaculty :: Int -> IO (Maybe [Group])
getGroupsInFaculty fid = getGroupsInFaculty' <$$> getGroupsRoot fid



allFacultiesIds' :: FacultiesRoot -> [Int]
allFacultiesIds' = (map faculty_id) . faculties

allFacultiesIds :: IO (Maybe [Int])
allFacultiesIds = allFacultiesIds' <$$> getFacultiesRoot



allGroups' :: [Int] -> IO (Maybe [GroupsRoot])
allGroups' ids= fmap sequence . sequence $ map getGroupsRoot ids


--allGroups :: IO (Maybe [(String, [(String, Int)])])



type SettingsRow = (String, String)
type Settings = [SettingsRow]
fromSetingsRow s = (k,v)
  where splitted = Split.splitOn "=" $ filter ((/=) ' ') s
        k = head splitted
        v = last splitted
toSetingsRow (k,v) = k ++ " = " ++ v ++ "\n"

findSetting :: String -> Settings -> Maybe String
findSetting key = fmap snd . List.find pred
    where pred (k,v) = k == key

parseSettings :: String -> Settings
parseSettings = (map fromSetingsRow) . (Split.splitOn "\n") . (filter $ (/=) ' ')

loadSettings :: FilePath -> IO Settings
loadSettings path = parseSettings <$> readFile path

saveSettings :: FilePath -> Settings -> IO ()
saveSettings path settings = writeFile path settings'
    where settings' = foldl (++) "" $ map toSetingsRow settings

executeCommand :: [String] -> IO String
executeCommand ("groups":[]) = do
  gs <- undefined
  return $ mconcat $ fmap (\s->T.unpack(s)++"\n") gs

selectFacIdIO :: IO Int
selectFacIdIO = do
  facs' <- faculties <$$> getFacultiesRoot
  let facs = case facs' of
        Nothing -> error "Unable to load from server"
        Just fs -> map pair fs
            where pair f = (faculty_name f, faculty_id f)
  let prettyPrintedFacs = concat $ map compact facs
        where compact (name, index) = "(" ++ show index ++ ") " ++name ++ "\n"

  putStrLn "(id) Name"
  putStrLn prettyPrintedFacs
  putStrLn "enter selected_fid"
  selected_fid <- getLine
  let fid = case readMaybe selected_fid :: Maybe Int of
        Just x -> x
        Nothing -> error "Unable to parse fid"

  unless (fid `elem` map snd facs) $ error "Unable to find fac"
  return fid

orderBy selector xs = sortBy comparison xs
        where comparison e1 e2 = uncurry compare $ join bimap selector (e1, e2)

selectGroupIdOnFacIO :: Int -> IO Int
selectGroupIdOnFacIO fid = do
  gs' <- groups <$$> getGroupsRoot fid
  let gs = case gs' of
        Nothing -> error "Unable to load from server"
        Just xs -> (orderBy fst . map pair) xs
            where pair g = (group_name g, group_id g)
  let simplifiedGids = zipWith (\real simple -> (simple, real)) (map snd gs) [1..]
  let prettyPrintedGroups = concat $ map compact gs
        where replaceIndex i = show $ fst $ fromJust $ find ((==) i . snd) simplifiedGids
              compact (name, index) = "(" ++ replaceIndex index ++ ") " ++name ++ "\n"

  putStrLn prettyPrintedGroups
  selected_sgid <- getLine
  let sgid = case readMaybe selected_sgid :: Maybe Int of
        Just x -> x
        Nothing -> error "Unable to parse sgid"

  let gid = case find (\(si, re) -> si == sgid) simplifiedGids of
        Just (si, re) -> re
        Nothing -> error $ "Cant find group " ++ show sgid
  return gid

main :: IO ()
main = do

  fid <- selectFacIdIO
  gid <- selectGroupIdOnFacIO fid

  putStrLn $ "Selected group " ++ show gid
