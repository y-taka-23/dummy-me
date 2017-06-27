{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , loadDummyDB
    , select
    , selectById
    ) where

import           Data.Aeson
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Vector            as V

type DummyDB = T.Text
type TopLevelKey = T.Text
type EntityId = Int

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB = TIO.readFile

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Value)
select x db = undefined

selectById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
selectById key id db =
    case select key db of
        (_, Just (Array records)) -> (db, V.find (idIs id) records)
        (_, _) -> (db, Nothing)

idIs :: EntityId -> Value -> Bool
idIs idNum (Object obj) =
    HM.lookup "id" obj == Just (Number (fromIntegral idNum))
idIs _ _ = False
