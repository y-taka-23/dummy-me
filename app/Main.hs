module Main where

import Web.DummyMe.DB
import Web.DummyMe.Handler

import Data.IORef
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
    dummyDB <- loadDummyDB "db.json"
    dbRef <- newIORef dummyDB
    spockCfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
    runSpock 8080 $ spock spockCfg $ do

        get var $ \key -> getHandler key
        get (var <//> var) $ \key id -> getByIdHandler key id

        delete (var <//> var) $ \key id -> deleteByIdHandler key id

        post var $ \key -> postHandler key

        put var $ \key -> putHandler key
        put (var <//> var) $ \key id -> putByIdHandler key id
