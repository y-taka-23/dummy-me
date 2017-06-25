{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Web.DummyMe.DB

import           Control.Monad.IO.Class
import           Data.IORef
import           Web.Spock
import           Web.Spock.Config

data InMemoryDB = InMemoryDB (IORef DummyDB)

main :: IO ()
main = do
    eDummyDB <- loadDummyDB "db.json"
    case eDummyDB of
        Left message -> do
            putStrLn message
        Right dummyDB -> do
            dbRef <- newIORef dummyDB
            spockCfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
            runSpock 8080 $ spock spockCfg $ do

                get var $ \key -> getAction key
                get (var <//> var) $ \key id -> getByIdAction key id

getAction key = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case select db key of
        Nothing -> error "unreachable code"
        Just val -> json val

getByIdAction key id = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case selectById db key id of
        Nothing -> error "unreachable code"
        Just val -> json val
