{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Text              as T
import           Web.Spock
import           Web.Spock.Config

main :: IO ()
main = do
    eDummyDB <- loadDummyDB "db.json"
    case eDummyDB of
        Left message -> do
            putStrLn message
        Right dummyDB -> do
            spockCfg <- defaultSpockCfg () PCNoDatabase dummyDB
            runSpock 8080 $ spock spockCfg $ do

                let route = ("sample" :: T.Text)
                get (static $ T.unpack route) $ do
                    text $ T.concat [ "Hello, ", route, "!" ]

type DummyDB = Object  -- HashMap Text Value
type Route = T.Text

loadDummyDB :: FilePath -> IO (Either String DummyDB)
loadDummyDB fp = eitherDecode' <$> BS.readFile fp
