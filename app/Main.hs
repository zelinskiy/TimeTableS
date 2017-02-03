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

jsonURL :: String
jsonURL = "http://cist.nure.ua/ias/app/tt/get_faculties"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String FacultiesRoot)
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
