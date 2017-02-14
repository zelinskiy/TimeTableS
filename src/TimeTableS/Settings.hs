module TimeTableS.Settings(
    Settings
  , loadSettings
  , saveSettings
  , findSetting
) where

import Data.List.Split as Split
import Data.List as List

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
