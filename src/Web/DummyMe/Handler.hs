{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.DummyMe.Handler (
      InMemoryDB(..)
    , getHandler
    , getByIdHandler
    , deleteByIdHandler
    , postHandler
    , putHandler
    , putByIdHandler
    , errorHandler
    , getDBHandler
    ) where

import Web.DummyMe.DB

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Maybe
import qualified Data.Text                  as T
import           Network.HTTP.Types.Status
import           Web.Spock

data InMemoryDB = InMemoryDB (IORef DummyDB)

getHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
getHandler key = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case select key db of
        (_, Nothing) -> errorHandler notFound404
        (_, Just ent) -> json ent

getByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
getByIdHandler key id = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case selectById key id db of
        (_, Nothing) -> errorHandler notFound404
        (_, Just ent) -> json ent

deleteByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                      HasSpock (ActionCtxT ctx m), MonadIO m) =>
                  TopLevelKey -> EntityId -> ActionCtxT ctx m b
deleteByIdHandler key id = do
    (InMemoryDB dbRef) <- getState
    mDeleted <- liftIO $ atomicModifyIORef' dbRef (deleteById key id)
    case mDeleted of
        Nothing -> errorHandler notFound404
        Just _ -> setStatus noContent204 >> json ""

postHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                HasSpock (ActionCtxT ctx m), MonadIO m) =>
            TopLevelKey -> ActionCtxT ctx m b
postHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mInserted <- liftIO $ atomicModifyIORef' dbRef (insert key entry)
    case mInserted of
        Nothing -> errorHandler notFound404
        Just ent -> do
            setStatus created201
            setLocation key ent
            json ent

putHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
putHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mUpdated <- liftIO $ atomicModifyIORef' dbRef (update key entry)
    case mUpdated of
        Nothing -> errorHandler notFound404
        Just _ -> setStatus noContent204 >> json ""

putByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
putByIdHandler key id = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mUpdated <- liftIO $ atomicModifyIORef' dbRef (updateById key id entry)
    case mUpdated of
        Nothing -> errorHandler notFound404
        Just _ -> setStatus noContent204 >> json ""

-- TODO: create JSON templates for each status
errorHandler :: (MonadIO m) => Status -> ActionCtxT ctx m b
errorHandler status = setStatus status >> json ""

-- TODO: better way of building the URI
formatLocation :: T.Text -> TopLevelKey -> EntityId -> T.Text
formatLocation host key id = T.concat [
      T.pack "http://", host
    , T.pack "/", key, T.pack "/", T.pack (show id)
    ]

setLocation :: MonadIO m => TopLevelKey -> Entity -> ActionCtxT ctx m ()
setLocation key ent = do
    mHost <- header (T.pack "Host")
    case (mHost, idOf ent) of
        (Just host, Just id) -> do
            setHeader (T.pack "Location") (formatLocation host key id)
        (_, _) -> error "unreachable"

getDBHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                 HasSpock (ActionCtxT ctx m), MonadIO m) =>
             ActionCtxT ctx m b
getDBHandler = do
    (InMemoryDB dbRef) <- getState
    json =<< liftIO (readIORef dbRef)
