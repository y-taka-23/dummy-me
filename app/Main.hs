{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson             hiding ( json )
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Text              as T
import           Web.Spock
import           Web.Spock.Config

main :: IO ()
main = do
    eDummyDB <- loadDummyDB "db.json"
    case eDummyDB of
        Left message -> do
            putStrLn message
        Right dummyDB -> do
            spockCfg <- defaultSpockCfg () PCNoDatabase dummyDB
            runSpock 8080 $ spock spockCfg $ do
                getState >>= getEndpointsOf

type DummyDB = Object  -- HashMap Text Value
type Route = T.Text

loadDummyDB :: FilePath -> IO (Either String DummyDB)
loadDummyDB fp = eitherDecode' <$> BS.readFile fp

routesOf :: DummyDB -> [Route]
routesOf = HM.keys

getEndpointsOf :: DummyDB -> SpockCtxM ctx conn sess st ()
getEndpointsOf db =
    let routes = routesOf db
    in mapM_ (getEndpoint db) routes

getEndpoint :: DummyDB -> Route -> SpockCtxM ctx conn sess st ()
getEndpoint db route = get (static $ T.unpack route) $ getAction db route

select :: T.Text -> DummyDB -> Maybe Value
select = HM.lookup

getAction :: (MonadIO m) => DummyDB -> T.Text -> ActionCtxT ctx m ()
getAction db key =
    case select key db of
        Nothing -> error "unreachable code"
        Just val -> json val
