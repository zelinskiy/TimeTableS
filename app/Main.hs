{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.Text hiding (drop, reverse, map, concat)
import Data.Text.Encoding as ENC
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import Control.Monad.IO.Class

import Lib
import TimeTableS.Types

{-
TODO:
Lenses
-}


fmap2 f = fmap (fmap f)
(<$$>) f x = f `fmap2` x
infixl 4 <$$>

getJSON :: String -> IO B.ByteString
getJSON url = simpleHttp url

allFacsUrl = "http://cist.nure.ua/ias/app/tt/get_faculties"
facUrl = "http://cist.nure.ua/ias/app/tt/get_groups?faculty_id="
timetableUrl = "http://cist.nure.ua/ias/app/tt/get_schedule?group_id="

getFacultiesRoot :: IO (Maybe FacultiesRoot)
getFacultiesRoot = decode <$> (getJSON allFacsUrl)

getTimetableRoot :: Int -> IO (Maybe TimeTable)
getTimetableRoot id = decode <$> (getJSON (timetableUrl ++ (show id)))

getGroupsRoot :: Int -> IO (Maybe GroupsRoot)
getGroupsRoot fid = decode <$> (getJSON (facUrl ++ (show fid)))

getGroupsInFaculty' :: GroupsRoot -> [Group]
getGroupsInFaculty' root = groups root

getGroupsInFaculty :: Int -> IO (Maybe [Group])
getGroupsInFaculty fid = getGroupsInFaculty' <$$> getGroupsRoot fid

allFacultiesIds' :: FacultiesRoot -> [Int]
allFacultiesIds' = (map faculty_id) . faculties

allFacultiesIds :: IO (Maybe [Int])
allFacultiesIds = allFacultiesIds' <$$> getFacultiesRoot

allGroups' :: [Int] -> IO (Maybe [GroupsRoot])
allGroups' ids = fmap sequence $ sequence $ (map (\id -> decode <$> (getJSON (facUrl ++ (show id))))) ids

allGroups = do
  fids' <- allFacultiesIds
  let fids = case fids' of
              Just xs -> xs
              Nothing -> []
  gs <- allGroups' fids
  let res = case gs of
              Just x -> x
              Nothing -> []
  return $ concat $ map ((map (group_name) ) . groups) res

main :: IO ()
main = do
  gs <- allGroups
  putStrLn $ mconcat $ fmap (\s->unpack(s)++"\n") gs
