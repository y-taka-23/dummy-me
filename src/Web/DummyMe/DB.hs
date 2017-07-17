module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , Entity(..)
    , loadDummyDB
    , select
    , selectById
    , deleteById
    , insert
    , update
    , updateById
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Scientific        as SCI
import qualified Data.Text              as T
import qualified Data.Vector            as V

newtype DummyDB = DummyDB BS.ByteString deriving ( Show )
type TopLevelKey = T.Text
type EntityId = Integer
type Entity = Value

instance Eq DummyDB where
    (==) (DummyDB x) (DummyDB y) =
        (decode x :: Maybe Entity) == (decode y :: Maybe Entity)

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB fp = DummyDB <$> BS.readFile fp

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Entity)
select x (DummyDB db) = (DummyDB db, db ^? key x)

-- TODO: It doen't go when there are multiple entities of the specified id
selectById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Entity)
selectById x n (DummyDB db) =
    (DummyDB db, db ^? key x . _Array . traverse . filtered (idIs n))

deleteById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Entity)
deleteById x n (DummyDB db) =
    let (_, mDeletedEntity) = selectById x n (DummyDB db)
    in  (DummyDB $ db & key x . _Array %~ purgeEntity n, mDeletedEntity)

purgeEntity :: EntityId -> V.Vector Entity -> V.Vector Entity
purgeEntity n = V.filter (not . idIs n)

insert :: TopLevelKey -> Entity -> DummyDB -> (DummyDB, Maybe Entity)
insert x ent (DummyDB db) =
    case nextId <$> db ^? key x . _Array of
        Nothing -> (DummyDB db, Nothing)
        Just newId ->
            let newEnt = ent & _Object %~ setId newId
            in  ( DummyDB $ db & key x . _Array %~ appendEntity newEnt
                , Just newEnt
                )

appendEntity :: Entity -> V.Vector Entity -> V.Vector Entity
appendEntity = flip V.snoc

setId :: EntityId -> Object -> Object
setId n = HM.insert (T.pack "id") (toJSON n)

nextId :: V.Vector Entity -> EntityId
nextId currents =
    case maximumOf (traverse . key (T.pack "id") . _Number) currents of
        Nothing -> 1
        Just sci -> case SCI.floatingOrInteger sci of
            Left _  -> 1
            Right n -> n + 1

update :: TopLevelKey -> Entity -> DummyDB -> (DummyDB, Maybe Entity)
update x ent (DummyDB db)
    | isSingular x (DummyDB db) = (DummyDB $ db & key x .~ ent, Just ent)
    | otherwise                 = (DummyDB db, Nothing)

isSingular :: TopLevelKey -> DummyDB -> Bool
isSingular x (DummyDB db) = case db ^? key x of
    Just (Array _) -> False
    Just _         -> True
    Nothing        -> False

updateById :: TopLevelKey -> EntityId -> Entity -> DummyDB
           -> (DummyDB, Maybe Entity)
updateById x n ent (DummyDB db)
    | DummyDB newDB == DummyDB db = (DummyDB db, Nothing)
    | otherwise   = (DummyDB newDB, Just newEnt)
    where
        newEnt = ent & _Object %~ setId n
        newDB  = db & key x . _Array %~ modifyEntity n newEnt

modifyEntity :: EntityId -> Entity -> V.Vector Entity -> V.Vector Entity
modifyEntity n ent currents =
    case V.findIndex (idIs n) currents of
        Nothing  -> currents
        Just idx -> currents V.// [(idx, ent)]

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key (T.pack "id") . _Integer == Just n
