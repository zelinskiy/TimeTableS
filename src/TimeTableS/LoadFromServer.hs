module TimeTableS.LoadFromServer (
    loadFaculties
  , loadGroups
  , loadDays
  ) where

import Data.Aeson (FromJSON, decode)
import Network.HTTP.Conduit (simpleHttp)
import TimeTableS.Types
import TimeTableS.Utils ((<$$>))

loadFaculties :: IO [Faculty]
loadFaculties = do
  facs <- faculties <$$> getFacultiesRoot
  case facs of
        Nothing -> error "Unable to load from server"
        Just fs -> return fs

loadGroups :: Int -> IO [Group]
loadGroups fid = do
  gs <- groups <$$> getGroupsRoot fid
  case gs of
        Nothing   -> error "Unable to load from server"
        Just res  -> return res
loadDays :: String -> IO [WeekDay]
loadDays gid = do
  tt <- getTimetableRoot gid
  case tt of
    Just x  -> return $ daysTT x
    Nothing -> error $ "cannot load timetable for gid " ++ gid

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
