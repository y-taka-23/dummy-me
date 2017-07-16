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
    ) where

import Web.DummyMe.DB

import Control.Monad.IO.Class
import Data.IORef
import Network.HTTP.Types.Status
import Web.Spock

data InMemoryDB = InMemoryDB (IORef DummyDB)

getHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
getHandler key = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case select key db of
        (_, Nothing) -> errorHandler notFound404
        (_, Just val) -> json val

getByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
getByIdHandler key id = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case selectById key id db of
        (_, Nothing) -> errorHandler notFound404
        (_, Just val) -> json val

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
        -- TODO: add Location header
        Just val -> setStatus created201 >> json val

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
