{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.DBSpec where

import Test.Hspec
import Web.DummyMe.DB

import           Control.Lens      hiding ( (.=) )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashSet      as HS
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
updStatusObjectDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": { \"prev\": \"started\", \"next\": \"stopped\" }, \"admin\": { \"name\":\"John\" } }"

updAliceDB, updBobDB :: DummyDB
updAliceDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Carol\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"
updBobDB   = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Carol\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

adminEmail, adminAltered :: Entity
adminEmail   = object [                          "email" .= String "john@example.com" ]
adminAltered = object [ "name" .= String "John", "email" .= String "john@example.com" ]

altAdminDB, altAliceDB :: DummyDB
altAdminDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\", \"email\":\"john@example.com\" } }"
altAliceDB = DummyDB "{ \"users\": [ { \"id\": 1, \"name\": \"Alice\",\"email\": \"john@example.com\" }, { \"id\": 2, \"name\": \"Bob\" } ], \"status\": \"test\", \"admin\": { \"name\":\"John\" } }"

spec :: Spec
spec = do

    describe "keySet" $ do
        it "should return two types of keys as the KeySet datatype" $ do
            (pluralKeys . keySet) db `shouldBe` HS.fromList ["users"]
            (singularKeys . keySet) db `shouldBe` HS.fromList ["status", "admin"]

    describe "select" $ do
        context "when the given key has an array of entries" $ do
            it "should return a vector of all entries" $ do
                select "users" db `shouldBe`
                    (db, Right (Array (alice `V.cons` V.singleton bob)))
        context "when the given key has a non-array entry" $ do
            it "should return a single entry" $ do
                select "status" db `shouldBe` (db, Right (String "test"))
        context "when the given key has no entry" $ do
            it "should return NoSuchEntity" $ do
                select "other" db `shouldBe` (db, Left NoSuchEntity)

    describe "selectById" $ do
        context "when the given key has an entry of the given id" $ do
            it "should return the single specified entry" $ do
                selectById "users" 1 db `shouldBe` (db, Right alice)
                selectById "users" 2 db `shouldBe` (db, Right bob)
        context "when the given key has no entry of the given id" $ do
            it "should return NoSuchEntity" $ do
                selectById "users" 3 db `shouldBe` (db, Left NoSuchEntity)
        context "when the given key has a non-array entry" $ do
            it "should return KeyTypeMismatch" $ do
                selectById "status" 1 db `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return NoSuchEntity" $ do
                selectById "other" 1 db `shouldBe` (db, Left NoSuchEntity)

    describe "deleteById" $ do
        context "when the given key has an entry of the given id" $ do
            it "should return a pair of the new DB and the deleted entry" $ do
                deleteById "users" 1 db `shouldBe` (delAliceDB, Right alice)
                deleteById "users" 2 db `shouldBe` (delBobDB, Right bob)
        context "when the give key has no entry of the given id" $ do
            it "should return a pair of the original db and NoSuchEntity" $ do
                deleteById "users" 3 db `shouldBe` (db, Left NoSuchEntity)
        context "when the given key has a non-array entry" $ do
            it "should return a pair of the original DB and KeyTypeMismatch" $ do
                deleteById "status" 1 db `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return a pair of the original DB and NoSuchEntity" $ do
                deleteById "other" 1 db `shouldBe` (db, Left NoSuchEntity)

    -- TODO: these test cases are fragile, i.e. depend on the numbering logic
    -- TODO: check that the numbering has no duplication
    describe "insert" $ do
        context "even when the given entry has the 'id' key" $ do
            it "should ignore the original id of the entry" $ do
                let (newDB, Right entry) = insert "users" carol db
                newDB `shouldBe` insCarolDB
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the given entry doesn't have the 'id' key" $ do
            it "should number the entry automatically" $ do
                let (newDB, Right entry) = insert "users" carolWithoutId db
                newDB `shouldBe` insCarolDB
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the gevin key has a non-array entry" $ do
            it "should return a pair of the original DB and KeyTypeMismatch" $ do
                insert "status" carol db `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return a pair of the original DB and NoSuchEntity" $ do
                insert "other" carol db `shouldBe` (db, Left NoSuchEntity)

    describe "update" $ do
        context "when the given key has a non-array entry" $ do
            it "should replace the entry and return the new entry" $ do
                let (newDB, Right entry) = update "status" statusString db
                newDB `shouldBe` updStatusStringDB
                entry `shouldBe` statusString
            it "should replace the entry and return the new entry" $ do
                let (newDB, Right entry) = update "status" statusObject db
                newDB `shouldBe` updStatusObjectDB
                entry `shouldBe` statusObject
        context "when the given key has an array of entries" $ do
            it "should return the original DB and KeyTypeMismatch" $ do
                update "users" statusString db
                    `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return the original DB and NoSuchEntity" $ do
                update "other" statusString db
                    `shouldBe` (db, Left NoSuchEntity)

    describe "updateById" $ do
        context "even when the given entry has the 'id' field" $ do
            it "should ignore the original id of the entry" $ do
                let (newDB, Right entry) = updateById "users" 1 carol db
                newDB `shouldBe` updAliceDB
                idOf entry `shouldBe` Just 1
                entry ^? key "name" `shouldBe` carol ^? key "name"
            it "should ignore the original id of the entry" $ do
                let (newDB, Right entry) = updateById "users" 2 carol db
                newDB `shouldBe` updBobDB
                idOf entry `shouldBe` Just 2
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the given entry doesn't have the 'id' key" $ do
            it "should update the entry by the specified id" $ do
                let (newDB, Right entry) = updateById "users" 1 carolWithoutId db
                newDB `shouldBe` updAliceDB
                idOf entry `shouldBe` Just 1
                entry ^? key "name" `shouldBe` carol ^? key "name"
            it "should update the entry by the specified id" $ do
                let (newDB, Right entry) = updateById "users" 2 carolWithoutId db
                newDB `shouldBe` updBobDB
                idOf entry `shouldBe` Just 2
                entry ^? key "name" `shouldBe` carol ^? key "name"
        context "when the give key has no entry of the given id" $ do
            it "should return a pair of the original DB and NoSuchEntity" $ do
                updateById "users" 3 carol db `shouldBe` (db, Left NoSuchEntity)
        context "when the given key has a non-array entry" $ do
            it "should return the original DB and KeyTypeMismatch" $ do
                updateById "status" 1 statusString db
                    `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return the original DB and NoSuchEntity" $ do
                updateById "other" 1 statusString db
                    `shouldBe` (db, Left NoSuchEntity)

    describe "alter" $ do
        -- TODO: add a test case for overwriting
        context "when the given key has a single Object entry" $ do
            it "should merge the entries, overwriting if necessary" $ do
                let (newDB, Right entity) = alter "admin" adminEmail db
                newDB `shouldBe` altAdminDB
                entity `shouldBe` adminAltered
        context "when the given key has a single non-Object entry" $ do
            it "should replace the entry with now one" $ do
                alter "status"  statusString db
                    `shouldBe` update "status" statusString db
                alter "status"  statusObject db
                    `shouldBe` update "status" statusObject db
        context "when the given key has an array of entries" $ do
            it "should return the original DB and KeyTypeMismatch" $ do
                alter "users" adminAltered db
                    `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $  do
            it "should return the original DB and NoSuchEntity" $ do
                alter "other" adminAltered db
                    `shouldBe` (db, Left NoSuchEntity)

    describe "alterById" $ do
        context "when the given key has an entry of the given id" $ do
            it "should update the entry partially" $ do
                let (newDB, Right entry) = alterById "users" 1 adminEmail db
                newDB `shouldBe` altAliceDB
                idOf entry `shouldBe` Just 1
                entry ^? key "name" `shouldBe` alice ^? key "name"
                entry ^? key "email" `shouldBe` adminEmail ^? key "email"
        context "when 'id' is specified in the given entry" $ do
            it "should ignore the 'id' and keep the original one" $ do
                alterById "users" 1 carol db
                    `shouldBe` updateById "users" 1 carol db
        context "when there is no entry of the given id" $ do
            it "should return the original DB and NoSuchEntity" $ do
                alterById "users" 3 adminEmail db
                    `shouldBe` (db, Left NoSuchEntity)
        context "when the given key has non-array enttry" $ do
            it "should return the original DB and KeyTypeMismatch" $ do
                alterById "status" 1 statusString db
                    `shouldBe` (db, Left KeyTypeMismatch)
        context "when the given key has no entry" $ do
            it "should return the original DB and NoSuchEntity" $ do
                alterById "other" 1 statusString db
                    `shouldBe` (db, Left NoSuchEntity)

    describe "idOf" $ do
        context "when the given entry has the 'id' field" $ do
            it "should return the value" $ do
                idOf alice `shouldBe` Just 1
                idOf bob `shouldBe` Just 2
        context "when the given entry doesn't have the 'id' field" $ do
            it "should return Nothing" $ do
                idOf carolWithoutId `shouldBe` Nothing
