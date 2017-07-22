{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe (
      runDummyMe
    ) where

import Web.DummyMe.DB
import Web.DummyMe.Config
import Web.DummyMe.Handler

import Data.IORef
import Web.Spock        hiding ( file )
import Web.Spock.Config

runDummyMe :: Config -> IO ()
runDummyMe appCfg = do
    dummyDB <- loadDummyDB $ file appCfg
    spockCfg <- mkSpockCfg dummyDB
    putStrLn $ "DummyMe is running on port " ++ show (port appCfg)
    runSpockNoBanner (port appCfg) $ spock spockCfg routes

mkSpockCfg :: DummyDB -> IO (SpockCfg () () InMemoryDB)
mkSpockCfg initDB = do
    dbRef <- newIORef initDB
    cfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
    return $ cfg { spc_errorHandler = errorHandler }

routes :: SpockCtxM ctx conn sess InMemoryDB ()
routes = do
    get    "_db"                       getDBHandler
    get     var           $ \key    -> getHandler key
    get    (var <//> var) $ \key id -> getByIdHandler key id
    delete (var <//> var) $ \key id -> deleteByIdHandler key id
    post    var           $ \key    -> postHandler key
    put     var           $ \key    -> putHandler key
    put    (var <//> var) $ \key id -> putByIdHandler key id
