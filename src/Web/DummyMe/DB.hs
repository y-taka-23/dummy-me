module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , Entity(..)
    , Schema(..)
    , loadDummyDB
    , dumpDummyDB
    , schema
    , select
    , selectById
    , deleteById
    , insert
    , update
    , updateById
    , idOf
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.List              as L
import           Data.Maybe
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

instance ToJSON DummyDB where
    toJSON (DummyDB db) = fromMaybe (error "unreachable") (decode db)

data Schema = Schema {
      pluralKeys   :: [TopLevelKey]
    , singularKeys :: [TopLevelKey]
    }

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB fp = DummyDB <$> BS.readFile fp

dumpDummyDB :: FilePath -> DummyDB -> IO ()
dumpDummyDB fp (DummyDB db) = BS.writeFile fp db

topLevelKeys :: DummyDB -> [TopLevelKey]
topLevelKeys (DummyDB db) = case decode db of
    Just (Object obj) -> HM.keys obj
    _                 -> error "unreachable"

schema :: DummyDB -> Schema
schema dummyDB =
    let (ss, ps) = L.partition (isSingular dummyDB) (topLevelKeys dummyDB)
    in  Schema { pluralKeys = ps, singularKeys = ss }

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
    | isSingular (DummyDB db) x = (DummyDB $ db & key x .~ ent, Just ent)
    | otherwise                 = (DummyDB db, Nothing)

isSingular :: DummyDB -> TopLevelKey -> Bool
isSingular (DummyDB db) x = case db ^? key x of
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

idOf :: Entity -> Maybe EntityId
idOf ent = ent ^? key (T.pack "id") . _Integer

idIs :: EntityId -> Entity -> Bool
idIs n ent = idOf ent == Just n
