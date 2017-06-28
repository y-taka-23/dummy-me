{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , loadDummyDB
    , select
    , selectById
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
selectById x n db = (db, db ^? key x ^.. folded . filtered (idIs n) ^? ix 0)

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key "id" . _Integer == Just n
