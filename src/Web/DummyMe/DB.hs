{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , loadDummyDB
    , select
    , selectById
    , deleteById
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

newtype DummyDB = DummyDB BS.ByteString deriving ( Show )
type TopLevelKey = T.Text
type EntityId = Integer

instance Eq DummyDB where
    (==) (DummyDB x) (DummyDB y) =
        (decode x :: Maybe Value) == (decode y :: Maybe Value)

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB fp = DummyDB <$> BS.readFile fp

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Value)
select x (DummyDB db) = (DummyDB db, db ^? key x)

selectById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
selectById x n (DummyDB db) =
    (DummyDB db, db ^? key x . _Array . traverse . filtered (idIs n))

deleteById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
deleteById x n (DummyDB db) =
    let (_, mDeletedEntity) = selectById x n (DummyDB db)
    in  (DummyDB $ db & key x %~ purgeEntity n, mDeletedEntity)

purgeEntity :: EntityId -> Value -> Value
purgeEntity n val =
    case val ^? _Array . traverse . filtered (not . idIs n) of
        Just newVal -> newVal
        Nothing     -> val

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key "id" . _Integer == Just n
