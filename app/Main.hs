module Main where

import           Web.DummyMe.DB

import           Control.Monad.IO.Class
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

type Route = T.Text

routesOf :: DummyDB -> [Route]
routesOf = HM.keys

getEndpointsOf :: DummyDB -> SpockCtxM ctx conn sess st ()
getEndpointsOf db =
    let routes = routesOf db
    in  mapM_ (getEndpoint db) routes

getEndpoint :: DummyDB -> Route -> SpockCtxM ctx conn sess st ()
getEndpoint db route = get (static $ T.unpack route) $ getAction db route

getAction :: (MonadIO m) => DummyDB -> T.Text -> ActionCtxT ctx m ()
getAction db key =
    case selectAll db key of
        Nothing -> error "unreachable code"
        Just val -> json val
