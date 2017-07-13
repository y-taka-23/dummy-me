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
    ) where

import Web.DummyMe.DB

import Control.Monad.IO.Class
import Data.IORef
import Web.Spock

data InMemoryDB = InMemoryDB (IORef DummyDB)

getHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
getHandler key = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case select key db of
        (_, Nothing) -> error "unreachable code"
        (_, Just val) -> json val

getByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
getByIdHandler key id = do
    (InMemoryDB dbRef) <- getState
    db <- liftIO $ readIORef dbRef
    case selectById key id db of
        (_, Nothing) -> error "unreachable code"
        (_, Just val) -> json val

deleteByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                      HasSpock (ActionCtxT ctx m), MonadIO m) =>
                  TopLevelKey -> EntityId -> ActionCtxT ctx m b
deleteByIdHandler key id = do
    (InMemoryDB dbRef) <- getState
    mDeleted <- liftIO $ atomicModifyIORef' dbRef (deleteById key id)
    case mDeleted of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"

postHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                HasSpock (ActionCtxT ctx m), MonadIO m) =>
            TopLevelKey -> ActionCtxT ctx m b
postHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mInserted <- liftIO $ atomicModifyIORef' dbRef (insert key entry)
    case mInserted of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"

putHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
               HasSpock (ActionCtxT ctx m), MonadIO m) =>
           TopLevelKey -> ActionCtxT ctx m b
putHandler key = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mUpdated <- liftIO $ atomicModifyIORef' dbRef (update key entry)
    case mUpdated of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"

putByIdHandler :: (SpockState (ActionCtxT ctx m) ~ InMemoryDB,
                   HasSpock (ActionCtxT ctx m), MonadIO m) =>
               TopLevelKey -> EntityId -> ActionCtxT ctx m b
putByIdHandler key id = do
    entry <- jsonBody' -- returns 400 on parsing error
    (InMemoryDB dbRef) <- getState
    mUpdated <- liftIO $ atomicModifyIORef' dbRef (updateById key id entry)
    case mUpdated of
        Nothing -> error "unreachable code"
        Just _ -> error "unreachable code"
