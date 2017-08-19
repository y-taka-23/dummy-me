{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe (
      runDummyMe
    ) where

import Web.DummyMe.DB
import Web.DummyMe.Config
import Web.DummyMe.Handler

import           Data.IORef
import           Data.Text        as T
import           Data.Version
import           Network.Wai.Middleware.RequestLogger
import qualified Paths_dummy_me
import           System.Directory
import           Web.Spock        hiding ( file )
import           Web.Spock.Config

runDummyMe :: Config -> IO ()
runDummyMe appCfg =
    if version appCfg
    then do
        putStrLn $ "DummyMe " ++ showVersion Paths_dummy_me.version
    else do
        printLogo
        putStrLn $ "Laoding database from " ++ file appCfg
        dummyDB <- loadDummyDB $ file appCfg
        printRoutes appCfg dummyDB
        createDirectoryIfMissing True $ snapshots appCfg
        spockCfg <- mkSpockCfg appCfg dummyDB
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
    patch   var           $ \key    -> patchHandler key

printLogo :: IO ()
printLogo = mapM_ putStrLn [
      " ____                                  __  __"
    , "|  _ \\ _   _ _ __ ___  _ __ ___  _   _|  \\/  | ___"
    , "| | | | | | | '_ ` _ \\| '_ ` _ \\| | | | |\\/| |/ _ \\"
    , "| |_| | |_| | | | | | | | | | | | |_| | |  | |  __/"
    , "|____/ \\__,_|_| |_| |_|_| |_| |_|\\__, |_|  |_|\\___|"
    , "_________________________________|___/_____________"
    , ""
    ]

printRoutes :: Config -> DummyDB -> IO ()
printRoutes appCfg dummyDB = do
    let ks = keySet dummyDB
    putStrLn ""
    mapM_ (putStrLn . showRoute (port appCfg)) $ pluralKeys ks
    mapM_ (putStrLn . showRoute (port appCfg)) $ singularKeys ks
    putStrLn ""

-- TODO: parameterize the host
showRoute :: Int -> TopLevelKey -> String
showRoute p x = "- http://localhost:" ++ show p ++ "/" ++ T.unpack x
