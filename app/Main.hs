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

{-
TODO:
Lenses

-}

data Faculty =
  Faculty { faculty_name  :: !Text
            , faculty_id  :: Int
            , date_start  :: !Text
            , date_end    :: !Text
            } deriving (Show,Generic)

instance FromJSON Faculty
instance ToJSON Faculty

data FacultiesRoot =
  FacultiesRoot { faculties  :: [Faculty] } deriving (Show,Generic)

instance FromJSON FacultiesRoot
instance ToJSON FacultiesRoot

data Group =
  Group { group_name  :: !Text
        , group_id  :: Int
        } deriving (Show,Generic)

instance FromJSON Group
instance ToJSON Group

data GroupsRoot =
  GroupsRoot { groups  :: [Group] } deriving (Show,Generic)

instance FromJSON GroupsRoot
instance ToJSON GroupsRoot

data Teacher = Teacher { teacher_name :: !Text } deriving (Show,Generic)

instance FromJSON Teacher
instance ToJSON Teacher

data Auditory =
  Auditory { auditory_name      :: !Text
            , auditory_address  :: !Text
            } deriving (Show,Generic)

instance FromJSON Auditory
instance ToJSON Auditory

data Lesson =
  Lesson {  _subject    :: !Text
          , _type       :: Int
          , _time_start :: String
          , _time_end   :: String
          , _date_start :: Maybe String
          , _date_end   :: Maybe String
          , _dates      :: Maybe [String]
          , _teachers   :: [Teacher]
          , _auditories :: [Auditory]
          } deriving (Show,Generic)

instance ToJSON Lesson where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Lesson where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data Day =
  Day { weekday  :: Int
      , lessons  :: [Lesson]
      } deriving (Show,Generic)

instance FromJSON Day
instance ToJSON Day

data TimeTable =
  TimeTable { group_nameTT  :: !Text
            , daysTT        :: [Day]
            } deriving (Show,Generic)

instance ToJSON TimeTable where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropLast 2 }

instance FromJSON TimeTable where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropLast 2 }

dropLast n = reverse . (drop n) . reverse

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
