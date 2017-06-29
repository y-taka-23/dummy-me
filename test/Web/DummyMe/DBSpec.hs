{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DBSpec where

import Test.Hspec
import Web.DummyMe.DB

import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector       as V

db :: DummyDB
db = "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\" }"

alice, bob :: Value
alice = Object $ HM.fromList [ ("id", Number 1), ("name", String "Alice") ]
bob   = Object $ HM.fromList [ ("id", Number 2), ("name", String "Bob") ]

spec :: Spec
spec = do

    describe "select" $ do
        context "when the given key has an array of entries" $ do
            it "should return a vector of all entries" $ do
                select "users" db `shouldBe`
                    (db, Just (Array (alice `V.cons` V.singleton bob)))
        context "when the given key has a non-array entry" $ do
            it "should return a single entry" $ do
                select "status" db `shouldBe` (db, Just (String "test"))
        context "when the given key has no entry" $ do
            it "should return Nothing" $ do
                select "other" db `shouldBe` (db, Nothing)
