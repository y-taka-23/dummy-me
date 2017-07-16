module Main where

import Web.DummyMe.DB
import Web.DummyMe.Handler
import Web.DummyMe.Config

import Data.IORef
import Web.Spock        hiding ( file )
import Web.Spock.Config

main :: IO ()
main = do
    appCfg <- getConfig
    dummyDB <- loadDummyDB $ file appCfg
    spockCfg <- mkSpockCfg dummyDB
    runSpockNoBanner (port appCfg) $ spock spockCfg routes

mkSpockCfg :: DummyDB -> IO (SpockCfg () () InMemoryDB)
mkSpockCfg initDB = do
    dbRef <- newIORef initDB
    cfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
    return $ cfg { spc_errorHandler = errorHandler }

routes :: SpockCtxM ctx conn sess InMemoryDB ()
routes = do
    get     var           $ \key    -> getHandler key
    get    (var <//> var) $ \key id -> getByIdHandler key id
    delete (var <//> var) $ \key id -> deleteByIdHandler key id
    post    var           $ \key    -> postHandler key
    put     var           $ \key    -> putHandler key
    put    (var <//> var) $ \key id -> putByIdHandler key id
