{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TimeTableS.Types
( Faculty
    , faculty_name
    , faculty_id
    , date_start
    , date_end
, FacultiesRoot
    , faculties
, Group
    , group_name
    , group_id
, GroupsRoot
    , groups
, Teacher
    , teacher_name
, Auditory
    , auditory_name
    , auditory_address
, Lesson
    , _subject
    , _type
    , _time_start
    , _time_end
    , _date_start
    , _date_end
    , _dates
    , _teachers
    , _auditories
, Day
    , weekday
    , lessons
, TimeTable
    , group_nameTT
    , daysTT
) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text hiding (drop, reverse, map, concat)
import Data.Text.Encoding as ENC


data Faculty =
  Faculty { faculty_name  :: String
            , faculty_id  :: Int
            , date_start  :: String
            , date_end    :: String
            } deriving (Show,Generic)

instance FromJSON Faculty
instance ToJSON Faculty

data FacultiesRoot =
  FacultiesRoot { faculties  :: [Faculty] } deriving (Show,Generic)

instance FromJSON FacultiesRoot
instance ToJSON FacultiesRoot

data Group =
  Group { group_name  :: String
        , group_id  :: Int
        } deriving (Show,Generic)

instance FromJSON Group
instance ToJSON Group

data GroupsRoot =
  GroupsRoot { groups  :: [Group] } deriving (Show,Generic)

instance FromJSON GroupsRoot
instance ToJSON GroupsRoot

data Teacher = Teacher { teacher_name :: String } deriving (Show,Generic)

instance FromJSON Teacher
instance ToJSON Teacher

data Auditory =
  Auditory { auditory_name      :: String
            , auditory_address  :: String
            } deriving (Show,Generic)

instance FromJSON Auditory
instance ToJSON Auditory

data Lesson =
  Lesson {  _subject    :: String
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
  TimeTable { group_nameTT  :: !String
            , daysTT        :: ![Day]
            } deriving (Show,Generic)

instance ToJSON TimeTable where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropLast 2 }

instance FromJSON TimeTable where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropLast 2 }

dropLast n = reverse . (drop n) . reverse
