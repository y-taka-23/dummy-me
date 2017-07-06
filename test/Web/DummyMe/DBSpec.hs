{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DBSpec where

import Test.Hspec
import Web.DummyMe.DB

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector       as V

db :: DummyDB
db = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\" }"

alice, bob :: Value
alice = Object $ HM.fromList [ ("id", Number 1), ("name", String "Alice") ]
bob   = Object $ HM.fromList [ ("id", Number 2), ("name", String "Bob") ]

delAliceDB, delBobDB :: DummyDB
delAliceDB = DummyDB "{ \"users\": [ { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\" }"
delBobDB   = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" } ], \"status\": \"test\" }"

carol, carolWithoutId :: Value
carol          = Object $ HM.fromList [ ("id", Number 0), ("name", String "Carol") ]
carolWithoutId = Object $ HM.fromList [ ("name", String "Carol") ]

insCarolDB :: DummyDB
insCarolDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" }, { \"id\": 3, \"name\": \"Carol\" } ], \"status\": \"test\" }"

statusString, statusObject :: Value
statusString = String "running"
statusObject = Object $ HM.fromList [ ("prev", String "started"), ("next", String "stopped") ]

updStatusStringDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"running\" }"
updStatusObjectDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": { \"prev\": \"started\", \"next\": \"stopped\" } }"

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

    -- TODO: these test cases are fragile, i.e. depend on the numbering logic
    -- TODO: check that the numbering has no duplication
    describe "insert" $ do
        context "even when the given entry has the 'id' key" $ do
            it "should ignore the original id of the entry" $ do
                let (newDB, Just entry) = insert "users" carol db
                newDB `shouldBe` insCarolDB
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the given entry doesn't have the 'id' key" $ do
            it "should number the entry automatically" $ do
                let (newDB, Just entry) = insert "users" carolWithoutId db
                newDB `shouldBe` insCarolDB
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the gevin key has a non-array entry" $ do
            it "should return a pair of the original DB and Nothing" $ do
                insert "status" carol db `shouldBe` (db, Nothing)
        context "when the given key has no entry" $ do
            it "should return a pair of the original DB and Nothing" $ do
                insert "other" carol db `shouldBe` (db, Nothing)

    describe "update" $ do
        context "when the given key has a non-array entry" $ do
            it "should replace the entry and return the new entry" $ do
                let (newDB, Just entry) = update "status" statusString db
                newDB `shouldBe` updStatusStringDB
                entry `shouldBe` statusString
            it "should replace the entry and return the new entry" $ do
                let (newDB, Just entry) = update "status" statusObject db
                newDB `shouldBe` updStatusObjectDB
                entry `shouldBe` statusObject
        context "when the given key has an array of entries" $ do
            it "should do nothing and return the original DB" $ do
                update "users" statusString db `shouldBe` (db, Nothing)
        context "when the given key has no entry" $ do
            it "should do nothing and return the original DB" $ do
                update "other" statusString db `shouldBe` (db, Nothing)
