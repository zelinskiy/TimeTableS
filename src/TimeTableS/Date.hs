module TimeTableS.Date (
    fromTimeTableFormat
  , getCurrentDay
) where

import TimeTableS.Utils (parseInt)

import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (
  Day,
  fromGregorianValid)

import Data.List.Split (splitOn)

getCurrentDay :: IO Day
getCurrentDay = return . utctDay =<< getCurrentTime

fromTimeTableFormat :: String -> Day
fromTimeTableFormat s = case sequence $ parseInt <$> splitOn "." s of
  Just (d:m:y:[]) -> case fromGregorianValid (toInteger y) m d of
    Just day -> day
    Nothing -> error $ "invalid date " ++ s
  _ -> error $ "invalid date " ++ s
