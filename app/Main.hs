module Main where

import Web.DummyMe.DB
import Web.DummyMe.Handler
import Web.DummyMe.Config

import Data.IORef
import Web.Spock        hiding ( file )
import Web.Spock.Config

main :: IO ()
main = do
    cfg <- getConfig
    dummyDB <- loadDummyDB $ file cfg
    dbRef <- newIORef dummyDB
    spockCfg <- defaultSpockCfg () PCNoDatabase (InMemoryDB dbRef)
    runSpock (port cfg) $ spock spockCfg $ do

        get var $ \key -> getHandler key
        get (var <//> var) $ \key id -> getByIdHandler key id

        delete (var <//> var) $ \key id -> deleteByIdHandler key id

        post var $ \key -> postHandler key

        put var $ \key -> putHandler key
        put (var <//> var) $ \key id -> putByIdHandler key id
