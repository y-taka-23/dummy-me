{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.DummyMe.DB

import           Control.Monad.IO.Class
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

                get var $ \key -> do
                    db <- getState
                    getAction db key

                get (var <//> var) $ \key id -> do
                    db <- getState
                    getByIdAction db key id

getAction :: (MonadIO m) => DummyDB -> TopLevelKey -> ActionCtxT ctx m ()
getAction db key =
    case select db key of
        Nothing -> error "unreachable code"
        Just val -> json val

getByIdAction :: (MonadIO m) => DummyDB -> TopLevelKey -> Int
              -> ActionCtxT ctx m ()
getByIdAction db key id =
    case selectById db key id of
        Nothing -> error "unreachable code"
        Just val -> json val
