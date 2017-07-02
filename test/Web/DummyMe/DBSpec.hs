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

delAliceDB, delBobDB :: DummyDB
delAliceDB = "{ \"users\": [ { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\" }"
delBobDB   = "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" } ], \"status\": \"test\" }"

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

    describe "selectById" $ do
        context "when the given key has an entry of the given id" $ do
            it "should return the single specified entry" $ do
                selectById "users" 1 db `shouldBe` (db, Just alice)
                selectById "users" 2 db `shouldBe` (db, Just bob)
        context "when the given key has no entry of the given id" $ do
            it "should return Nothing" $ do
                selectById "users" 3 db `shouldBe` (db, Nothing)
        context "when the given key has a non-array entry" $ do
            it "should return Nothing" $ do
                selectById "status" 1 db `shouldBe` (db, Nothing)
        context "when the given key has no entry" $ do
            it "should return Nothing" $ do
                selectById "other" 1 db `shouldBe` (db, Nothing)

    describe "deleteById" $ do
        context "when the given key has an entry of the given id" $ do
            it "should return a pair of the new DB and the deleted entry" $ do
                deleteById "users" 1 db `shouldBe` (delAliceDB, Just alice)
                deleteById "users" 2 db `shouldBe` (delBobDB, Just bob)
        context "when the give key has no entry of the given id" $ do
            it "should return a pair of the original DB and Nothing" $ do
                deleteById "users" 3 db `shouldBe` (db, Nothing)
        context "when the given key has a non-array entry" $ do
            it "should return a pair of the original DB and Nothing" $ do
                deleteById "status" 1 db `shouldBe` (db, Nothing)
        context "when the given key has no entry" $ do
            it "should return a pair of the original DB and Nothing" $ do
                deleteById "other" 1 db `shouldBe` (db, Nothing)
