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
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

type DummyDB = T.Text
type TopLevelKey = T.Text
type EntityId = Integer

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB = TIO.readFile

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Value)
select x db = (db, db ^? key x)

selectById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
selectById x n db = (db, db ^? key x . _Array . traverse . filtered (idIs n))

deleteById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
deleteById x n db =
    let (_, mDeletedEntity) = selectById x n db
    in  (db & key x %~ purgeEntity n, mDeletedEntity)

purgeEntity :: EntityId -> Value -> Value
purgeEntity n val =
    case val ^? _Array . traverse . filtered (not . idIs n) of
        Just newVal -> newVal
        Nothing     -> val

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key "id" . _Integer == Just n
