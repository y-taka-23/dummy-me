{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DB (
      DummyDB(..)
    , loadDummyDB
    , selectAll
    , selectById
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Text              as T
import qualified Data.Vector            as V

type DummyDB = Object  -- HashMap Text Value

loadDummyDB :: FilePath -> IO (Either String DummyDB)
loadDummyDB fp = eitherDecode' <$> BS.readFile fp

selectAll :: DummyDB -> T.Text -> Maybe Value
selectAll = flip HM.lookup

selectById :: DummyDB -> T.Text -> Int -> Maybe Value
selectById db table key =
    case selectAll db table of
        Just (Array records) -> V.find (idIs key) records
        _ -> Nothing

idIs :: Int -> Value -> Bool
idIs key (Object obj) =
    HM.lookup "id" obj == Just (Number (fromIntegral key))
isId _ _ = False
