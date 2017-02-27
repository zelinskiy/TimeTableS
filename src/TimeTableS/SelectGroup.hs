module TimeTableS.SelectGroup (
  selectGroupIO
) where

import TimeTableS.Types
import TimeTableS.LoadFromServer
import TimeTableS.Utils (orderBy)
import TimeTableS.Settings(saveSettings)

import Data.List as List
import Text.Read
import Control.Monad


withShallowIds :: [(Int, a)] -> [((Int, Int), a)]
withShallowIds rows = res
  where realIds       = map fst rows
        data_         = map snd rows
        withSimpleIds = zipWith (\re si -> (re, si)) realIds [1..]
        res           = zipWith (,) withSimpleIds data_

idWithName :: [(Int, String)] -> String
idWithName rows = concat $ map compact rows
  where compact (index, name) = "(" ++ show index ++ ") " ++name ++ "\n"


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
        Just x  -> x
        Nothing -> error "Unable to parse fid"

  unless (fid `elem` map faculty_id facs) $ error "Unable to find fac"
  return fid

--------------------------------------------------------------------------------

showGroupsWithIds :: [(Int, Group)] -> IO ()
showGroupsWithIds = putStrLn . concat . (pp <$>)
  where pp (i, g) = "("++show i ++")"++(group_name g)++"\n"

selectGroupIdOnFacIO :: Int -> IO Int
selectGroupIdOnFacIO fid = do
  gs <- orderBy group_name <$> loadGroups fid

  let pairs = (\g -> (group_id g, group_name g)) <$> gs
  let _withShallowIds = withShallowIds pairs
  let simplifiedGids = map fst _withShallowIds
  showGroupsWithIds $ zipWith (,) (snd <$> simplifiedGids) gs

  gid1 <- getLine

  let gid2 = case readMaybe gid1 :: Maybe Int of
        Just x -> x
        Nothing -> error $ "Unable to parse sgid " ++ gid1

  let gid3 = case find (\(_, si) -> si == gid2) simplifiedGids of
        Just (re, _) -> re
        Nothing -> error $ "Cant find group " ++ show gid2
  return gid3

selectGroupIO :: IO ()
selectGroupIO = do
  fid <- selectFacId
  gid <- selectGroupIdOnFacIO fid
  putStrLn $ "Selected group " ++ show gid
  saveSettings "settings.txt" [("group_id", show gid)]
