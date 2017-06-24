{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , loadDummyDB
    , topLevelKeys
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

topLevelKeys :: DummyDB -> [TopLevelKey]
topLevelKeys = HM.keys

select :: DummyDB -> TopLevelKey -> Maybe Value
select = flip HM.lookup

selectById :: DummyDB -> TopLevelKey -> Int -> Maybe Value
selectById db key idNum =
    case select db key of
        Just (Array records) -> V.find (idIs idNum) records
        _ -> Nothing

idIs :: Int -> Value -> Bool
idIs idNum (Object obj) =
    HM.lookup "id" obj == Just (Number (fromIntegral idNum))
isId _ _ = False
