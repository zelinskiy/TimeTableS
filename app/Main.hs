{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import Lib

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

allFacsUrl = "http://cist.nure.ua/ias/app/tt/get_faculties"
csFacUrl = "http://cist.nure.ua/ias/app/tt/get_groups?faculty_id=95"

getJSON :: String -> IO B.ByteString
getJSON url = simpleHttp url

getFacultiesRoot :: IO (Either String FacultiesRoot)
getFacultiesRoot = eitherDecode <$> (getJSON allFacsUrl)

main :: IO ()
main = do
 d <- getFacultiesRoot
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
