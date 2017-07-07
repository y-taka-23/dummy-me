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
    dummyDB <- loadDummyDB "db.json"
    dbRef <- newIORef dummyDB
    spockCfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
    runSpock 8080 $ spock spockCfg $ do

        get var $ \key -> getAction key
        get (var <//> var) $ \key id -> getByIdAction key id

        delete (var <//> var) $ \key id -> deleteByIdAction key id

        post var $ \key -> postAction key

getAction key = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case select key db of
        (_, Nothing) -> error "unreachable code"
        (_, Just val) -> json val

getByIdAction key id = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case selectById key id db of
        (_, Nothing) -> error "unreachable code"
        (_, Just val) -> json val

deleteByIdAction key id = do
    (InMemoryDB dbRef) <- getState
    mDeleted <- liftIO $ atomicModifyIORef' dbRef (deleteById key id)
    case mDeleted of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"

postAction key = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mInserted <- liftIO $ atomicModifyIORef' dbRef (insert key entry)
    case mInserted of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"
