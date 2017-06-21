{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.Text          as T
import           Web.Spock
import           Web.Spock.Config

main :: IO ()
main = do
    dummyDB <- readFile "db.json"
    spockCfg <- defaultSpockCfg () PCNoDatabase dummyDB
    runSpock 8080 $ spock spockCfg $ do

        get root $ do
            db <- getState
            text $ T.pack db
