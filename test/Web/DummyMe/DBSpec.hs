{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DBSpec where

import Test.Hspec
import Web.DummyMe.DB

import           Control.Lens      hiding ( (.=) )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Vector       as V

db :: DummyDB
db = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

alice, bob :: Entity
alice = object [ "id" .= Number 1, "name" .= String "Alice" ]
bob   = object [ "id" .= Number 2, "name" .= String "Bob" ]

delAliceDB, delBobDB :: DummyDB
delAliceDB = DummyDB "{ \"users\": [ { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"
delBobDB   = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

carol, carolWithoutId :: Entity
carol          = object [ "id" .= Number 0, "name" .= String "Carol" ]
carolWithoutId = object [ "name" .= String "Carol" ]

insCarolDB :: DummyDB
insCarolDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" }, { \"id\": 3, \"name\": \"Carol\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

statusString, statusObject :: Entity
statusString = String "running"
statusObject = object [ "prev" .= String "started", "next" .= String "stopped" ]

updStatusStringDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"running\", \"admin\": { \"name\":\"John\" } }"
updStatusObjectDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": { \"prev\": \"started\", \"next\": \"stopped\", \"admin\": { \"name\":\"John\" } } }"

updAliceDB, updBobDB :: DummyDB
updAliceDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Carol\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"
updBobDB   = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Carol\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

spec :: Spec
spec = do

    -- TODO: it should not be order-sensitive
    describe "keySet" $ do
        it "should return two types of keys as the KeySet datatype" $ do
            (pluralKeys . keySet) db `shouldBe` ["users"]
            (singularKeys . keySet) db `shouldBe` ["status", "admin"]

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

    describe "updateById" $ do
        context "even when the given entry has the 'id' field" $ do
            it "should ignore the original id of the entry" $ do
                let (newDB, Just entry) = updateById "users" 1 carol db
                newDB `shouldBe` updAliceDB
                idOf entry `shouldBe` Just 1
                entry ^? key "name" `shouldBe` carol ^? key "name"
            it "should ignore the original id of the entry" $ do
                let (newDB, Just entry) = updateById "users" 2 carol db
                newDB `shouldBe` updBobDB
                idOf entry `shouldBe` Just 2
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the given entry doesn't have the 'id' key" $ do
            it "should update the entry by the specified id" $ do
                let (newDB, Just entry) = updateById "users" 1 carolWithoutId db
                newDB `shouldBe` updAliceDB
                idOf entry `shouldBe` Just 1
                entry ^? key "name" `shouldBe` carol ^? key "name"
            it "should update the entry by the specified id" $ do
                let (newDB, Just entry) = updateById "users" 2 carolWithoutId db
                newDB `shouldBe` updBobDB
                idOf entry `shouldBe` Just 2
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the given key has a non-array entry" $ do
            it "should do nothing and return the original DB" $ do
                updateById "status" 1 statusString db `shouldBe` (db, Nothing)
        context "when the given key has no entry" $ do
            it "should do nothing and return the original DB" $ do
                updateById "other" 1 statusString db `shouldBe` (db, Nothing)

    describe "idOf" $ do
        context "when the given entry has the 'id' field" $ do
            it "should return the value" $ do
                idOf alice `shouldBe` Just 1
                idOf bob `shouldBe` Just 2
        context "when the given entry doesn't have the 'id' field" $ do
            it "should return Nothing" $ do
                idOf carolWithoutId `shouldBe` Nothing
