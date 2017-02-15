module TimeTableS.Date (
    Date
  , fromTimeTableFormat
  , rangeDates
  , getCurrentDate
  , addDaysToDate
) where

import TimeTableS.Utils (parseInt)

import Data.Time.Clock (utctDay, getCurrentTime)
import qualified Data.Time.Calendar as Cal (
  Day,
  toGregorian,
  fromGregorian,
  addDays,
  fromGregorianValid)
import Data.List.Split (splitOn)

data Date = Date Int Int Int -- :: Date Year Month Day
  deriving (Eq)

instance Show Date where
  show (Date y m d) = show d ++ "." ++ show m ++ "." ++ show y

toDay :: Date -> Cal.Day
toDay (Date y m d) = Cal.fromGregorian (toInteger y) m d

fromDay :: Cal.Day -> Date
fromDay day = case Cal.toGregorian day of
        (y, m, d) -> Date (fromInteger y) m d

getCurrentDate :: IO Date
getCurrentDate = do
  time' <- getCurrentTime
  return $ fromDay <$> utctDay $ time'

fromTimeTableFormat :: String -> Either String Date
fromTimeTableFormat s = case splitOn "." s of
  d:m:y:[] -> case parseInt d of
    Just day -> case parseInt m of
      Just month -> case parseInt y of
        Just year -> Right (Date year month day)
        Nothing -> Left $ "Unable to parse year " ++ y
      Nothing -> Left $ "Unable to parse month " ++ m
    Nothing -> Left  $ "Unable to parse day " ++ d
  _ -> Left $ "Inconsistent date format " ++ s

addDaysToDate :: Int -> Date -> Date
addDaysToDate n = fromDay . Cal.addDays (toInteger n) . toDay

rangeDates' acc begin end step
  | begin == end = [begin]
  | (toDay begin) > (toDay end) = error "Begin date must preceed end date"
  | (toDay res) < (toDay end) = rangeDates' (acc++[res]) begin end step
  | otherwise = [begin] ++ acc ++ [end]
    where curstep = step * (length acc + 1)
          res = addDaysToDate curstep begin

rangeDates = rangeDates' []
