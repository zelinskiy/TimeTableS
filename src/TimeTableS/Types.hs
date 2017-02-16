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
, WeekDay
    , weekday
    , lessons
, TimeTable
    , group_nameTT
    , daysTT
, showTypeLesson
, showShortTypeLesson
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

showTypeLesson :: Int -> String
showTypeLesson i = case i of
  0  -> "Практическое занятие"
  1  -> "Лабораторная работа"
  2  -> "Лекция"
  3  -> "Семинар"
  4  -> "Консультация"
  5  -> "Самостоятельная работа студента"
  6  -> "Зачет"
  7  -> "Экзамен"
  8  -> "Презентация"
  9  -> "Мастер-класс"
  10 -> "День открытых дверей"
  11 -> "Экскурсия"
  12 -> "Фильм"
  13 -> "Концерт, научное шоу"
  14 -> "Конкурс"
  15 -> "Конференция, научная школа"
  16 -> "Круглый стол"
  17 -> "Олимпиада"
  18 -> "Выставка"
  19 -> "Курсовая работа"
  _  -> "Unknown type"

showShortTypeLesson :: Int -> String
showShortTypeLesson i = case i of
  0  -> "ПЗ"
  1  -> "ЛР"
  2  -> "ЛК"
  4  -> "КС"
  7  -> "ЭК"
  _  -> "??"

instance ToJSON Lesson where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Lesson where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data WeekDay =
  WeekDay { weekday  :: Int
      , lessons  :: [Lesson]
      } deriving (Show,Generic)

instance FromJSON WeekDay
instance ToJSON WeekDay

data TimeTable =
  TimeTable { group_nameTT  :: !String
            , daysTT        :: ![WeekDay]
            } deriving (Show,Generic)

instance ToJSON TimeTable where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropLast 2 }

instance FromJSON TimeTable where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropLast 2 }

dropLast n = reverse . (drop n) . reverse
