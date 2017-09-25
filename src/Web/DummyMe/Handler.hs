{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.DummyMe.Handler (
      AppState(..)
    , mkAppState
    , getHandler
    , getByIdHandler
    , deleteByIdHandler
    , postHandler
    , putHandler
    , putByIdHandler
    , patchHandler
    , patchByIdHandler
    , errorHandler
    , getDBHandler
    , postSnapshotHandler
    ) where

import Web.DummyMe.Config
import Web.DummyMe.DB
import Web.DummyMe.Response

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                  as T
import           Network.HTTP.Types.Status
import           System.FilePath.Posix
import           System.Posix
import           Web.Spock

data AppState = AppState {
      config :: Config
    , inMemoryDB :: IORef DummyDB
    }

mkAppState :: Config -> IORef DummyDB -> AppState
mkAppState appCfg dbRef = AppState {
      config = appCfg
    , inMemoryDB = dbRef
    }

getHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
getHandler key = do
    dbRef <- inMemoryDB <$> getState
    db <- liftIO $ readIORef dbRef
    case select key db of
        (_, Left NoSuchEntity)    -> errorHandler notFound404
        (_, Left KeyTypeMismatch) -> error "unreachable"
        (_, Right ent)            -> json ent

getByIdHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
getByIdHandler key id = do
    dbRef <- inMemoryDB <$> getState
    db <- liftIO $ readIORef dbRef
    case selectById key id db of
        (_, Left NoSuchEntity)    -> errorHandler notFound404
        (_, Left KeyTypeMismatch) -> errorHandler badRequest400
        (_, Right ent)            -> json ent

deleteByIdHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                      HasSpock (ActionCtxT ctx m), MonadIO m) =>
                  Identifier -> TopLevelKey -> EntityId -> ActionCtxT ctx m ()
deleteByIdHandler ident key id = do
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (deleteById ident key id)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right _              -> setStatus noContent204

postHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                HasSpock (ActionCtxT ctx m), MonadIO m) =>
            TopLevelKey -> ActionCtxT ctx m b
postHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (insert key entry)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right ent -> do
            setStatus created201
            setLocation key ent
            json ent

putHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m ()
putHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (update key entry)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right _              -> setStatus noContent204

putByIdHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m ()
putByIdHandler key id = do
    entry <- jsonBody' -- returns 400 on parsing error
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (updateById key id entry)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right _              -> setStatus noContent204

patchHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                 HasSpock (ActionCtxT ctx m), MonadIO m) =>
             TopLevelKey -> ActionCtxT ctx m ()
patchHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (alter key entry)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right _              -> setStatus noContent204

patchByIdHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m ()
patchByIdHandler key id = do
    entry <- jsonBody' -- returns 400 on parsing error
    dbRef <- inMemoryDB <$> getState
    eResult <- liftIO $ atomicModifyIORef' dbRef (alterById key id entry)
    case eResult of
        Left NoSuchEntity    -> errorHandler notFound404
        Left KeyTypeMismatch -> errorHandler badRequest400
        Right _              -> setStatus noContent204

errorHandler :: (MonadIO m) => Status -> ActionCtxT ctx m b
errorHandler status = setStatus status >> json (renderStatus status)

-- TODO: better way of building the URI
formatLocation :: T.Text -> TopLevelKey -> EntityId -> T.Text
formatLocation host key id = T.concat [
      T.pack "http://", host
    , T.pack "/", key, T.pack "/", T.pack (show id)
    ]

setLocation :: MonadIO m => TopLevelKey -> Entity -> ActionCtxT ctx m ()
setLocation key ent = do
    mHost <- header (T.pack "Host")
    case (mHost, idOf (T.pack "id") ent) of
        (Just host, Just id) -> do
            setHeader (T.pack "Location") (formatLocation host key id)
        (_, _) -> error "unreachable"

getDBHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                 HasSpock (ActionCtxT ctx m), MonadIO m) =>
             ActionCtxT ctx m b
getDBHandler = do
    dbRef <- inMemoryDB <$> getState
    json =<< liftIO (readIORef dbRef)

postSnapshotHandler :: (SpockState (ActionCtxT ctx m) ~ AppState,
                        HasSpock (ActionCtxT ctx m), MonadIO m) =>
                    ActionCtxT ctx m ()
postSnapshotHandler = do
    appState <- getState
    liftIO $ do
        time <- epochTime
        db <- readIORef $ inMemoryDB appState
        dumpDummyDB (snapshotFilePath (config appState) time) db
    setStatus noContent204

snapshotFilePath :: Config -> EpochTime -> FilePath
snapshotFilePath appCfg time = addExtension snapshotName "json"
    where
        snapshotName = snapshots appCfg </> "snapshot-" ++ show time
