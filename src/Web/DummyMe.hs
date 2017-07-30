{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe (
      runDummyMe
    ) where

import Web.DummyMe.DB
import Web.DummyMe.Config
import Web.DummyMe.Handler

import           Data.IORef
import           Data.Version
import           Network.Wai.Middleware.RequestLogger
import qualified Paths_dummy_me
import           Web.Spock        hiding ( file )
import           Web.Spock.Config

runDummyMe :: Config -> IO ()
runDummyMe appCfg =
    if version appCfg
    then do
        putStrLn $ "DummyMe " ++ showVersion Paths_dummy_me.version
    else do
        dummyDB <- loadDummyDB $ file appCfg
        spockCfg <- mkSpockCfg appCfg dummyDB
        putStrLn $ "DummyMe is running on port " ++ show (port appCfg)
        runSpockNoBanner (port appCfg) $
            fmap (logStdoutDev .) $ spock spockCfg routes

mkSpockCfg :: Config -> DummyDB -> IO (SpockCfg () () AppState)
mkSpockCfg appCfg initDB = do
    dbRef <- newIORef initDB
    cfg <- defaultSpockCfg () PCNoDatabase $ mkAppState appCfg dbRef
    return $ cfg { spc_errorHandler = errorHandler }

routes :: SpockCtxM ctx conn sess AppState ()
routes = do
    get    "_db"                       getDBHandler
    post   "_snapshot"                 postSnapshotHandler
    get     var           $ \key    -> getHandler key
    get    (var <//> var) $ \key id -> getByIdHandler key id
    delete (var <//> var) $ \key id -> deleteByIdHandler key id
    post    var           $ \key    -> postHandler key
    put     var           $ \key    -> putHandler key
    put    (var <//> var) $ \key id -> putByIdHandler key id
