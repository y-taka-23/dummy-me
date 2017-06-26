{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , loadDummyDB
    , select
    , selectById
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Text              as T
import qualified Data.Vector            as V

type DummyDB = Object  -- HashMap Text Value
type TopLevelKey = T.Text

loadDummyDB :: FilePath -> IO (Either String DummyDB)
loadDummyDB fp = eitherDecode' <$> BS.readFile fp

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Value)
select key db = (db, HM.lookup key db)

selectById :: TopLevelKey -> Int -> DummyDB -> (DummyDB, Maybe Value)
selectById key id db =
    case select key db of
        (_, Just (Array records)) -> (db, V.find (idIs id) records)
        (_, _) -> (db, Nothing)

idIs :: Int -> Value -> Bool
idIs idNum (Object obj) =
    HM.lookup "id" obj == Just (Number (fromIntegral idNum))
idIs _ _ = False
