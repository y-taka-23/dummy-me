module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , Entity(..)
    , Identifier(..)
    , KeySet(..)
    , QueryError(..)
    , loadDummyDB
    , dumpDummyDB
    , keySet
    , select
    , selectById
    , deleteById
    , insert
    , update
    , updateById
    , alter
    , alterById
    , idOf
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as HM
import qualified Data.HashSet           as HS
import qualified Data.List              as L
import           Data.Maybe
import qualified Data.Scientific        as SCI
import qualified Data.Text              as T
import qualified Data.Vector            as V

newtype DummyDB = DummyDB BS.ByteString deriving ( Show )
type TopLevelKey = T.Text
type EntityId = Integer
type Entity = Value
type Identifier = T.Text

instance Eq DummyDB where
    (==) (DummyDB x) (DummyDB y) =
        (decode x :: Maybe Entity) == (decode y :: Maybe Entity)

instance ToJSON DummyDB where
    toJSON (DummyDB db) = fromMaybe (error "unreachable") (decode db)

data KeySet = KeySet {
      pluralKeys   :: HS.HashSet TopLevelKey
    , singularKeys :: HS.HashSet TopLevelKey
    }

data QueryError =
      NoSuchEntity
    | KeyTypeMismatch
    deriving ( Eq, Show )

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB fp = DummyDB <$> BS.readFile fp

dumpDummyDB :: FilePath -> DummyDB -> IO ()
dumpDummyDB fp (DummyDB db) = BS.writeFile fp db

topLevelKeys :: DummyDB -> [TopLevelKey]
topLevelKeys (DummyDB db) = case decode db of
    Just (Object obj) -> HM.keys obj
    _                 -> error "unreachable"

keySet :: DummyDB -> KeySet
keySet dummyDB =
    let (ss, ps) = L.partition (isSingular dummyDB) (topLevelKeys dummyDB)
    in  KeySet { pluralKeys = HS.fromList ps, singularKeys = HS.fromList ss }

select :: TopLevelKey -> DummyDB -> (DummyDB, Either QueryError Entity)
select x (DummyDB db) = case db ^? key x of
    Nothing  -> (DummyDB db, Left NoSuchEntity)
    Just ent -> (DummyDB db, Right ent)

-- TODO: It doen't go when there are multiple entities of the specified id
selectById :: Identifier -> TopLevelKey -> EntityId -> DummyDB
           -> (DummyDB, Either QueryError Entity)
selectById ident x n (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | isPlural   (DummyDB db) x =
        case db ^? key x . _Array . traverse . filtered (ident `is` n) of
            Nothing  -> (DummyDB db, Left NoSuchEntity)
            Just ent -> (DummyDB db, Right ent)
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

deleteById :: Identifier -> TopLevelKey -> EntityId -> DummyDB
           -> (DummyDB, Either QueryError Entity)
deleteById ident x n (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | isPlural   (DummyDB db) x = case selectById (T.pack "id") x n (DummyDB db) of
        (_, Left NoSuchEntity)    -> (DummyDB db, Left NoSuchEntity)
        (_, Left KeyTypeMismatch) -> (DummyDB db, Left KeyTypeMismatch)
        (_, Right ent )           -> (DummyDB newDB, Right ent)
            where newDB = db & key x . _Array %~ purgeEntity ident n
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

purgeEntity :: Identifier -> EntityId -> V.Vector Entity -> V.Vector Entity
purgeEntity ident n = V.filter $ not . (ident `is` n)

insert :: TopLevelKey -> Entity -> DummyDB
       -> (DummyDB, Either QueryError Entity)
insert x ent (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | isPlural   (DummyDB db) x = case nextId <$> db ^? key x . _Array of
        Nothing    -> (DummyDB db, Left NoSuchEntity)
        Just newId ->
            let newEnt = setId newId ent
                newDB  = db & key x . _Array %~ appendEntity newEnt
            in  (DummyDB newDB, Right newEnt)
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

appendEntity :: Entity -> V.Vector Entity -> V.Vector Entity
appendEntity = flip V.snoc

setId :: EntityId -> Entity -> Entity
setId n (Object obj) = Object $ HM.insert (T.pack "id") (toJSON n) obj
setId _ ent          = ent

nextId :: V.Vector Entity -> EntityId
nextId currents =
    case maximumOf (traverse . key (T.pack "id") . _Number) currents of
        Nothing -> 1
        Just sci -> case SCI.floatingOrInteger sci of
            Left _  -> 1
            Right n -> n + 1

update :: TopLevelKey -> Entity -> DummyDB
       -> (DummyDB, Either QueryError Entity)
update x ent (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB $ db & key x .~ ent, Right ent)
    | isPlural   (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

isSingular :: DummyDB -> TopLevelKey -> Bool
isSingular (DummyDB db) x = case db ^? key x of
    Just (Array _) -> False
    Just _         -> True
    Nothing        -> False

isPlural :: DummyDB -> TopLevelKey -> Bool
isPlural (DummyDB db) x = case db ^? key x of
    Just (Array _) -> True
    _              -> False

updateById :: TopLevelKey -> EntityId -> Entity -> DummyDB
           -> (DummyDB, Either QueryError Entity)
updateById x n ent (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | isPlural   (DummyDB db) x = case selectById (T.pack "id") x n (DummyDB db) of
        (_, Left err) -> (DummyDB db, Left err)
        (_, Right _)  -> (DummyDB newDB, Right newEnt)
            where
                newEnt = setId n ent
                newDB  = db & key x . _Array %~ modifyEntity n newEnt
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

modifyEntity :: EntityId -> Entity -> V.Vector Entity -> V.Vector Entity
modifyEntity n ent currents =
    case V.findIndex (idIs n) currents of
        Nothing  -> currents
        Just idx -> currents V.// [(idx, ent)]

-- TODO: more suitable naming
-- TODO: the logic traverses the DB string twice
alter :: TopLevelKey -> Entity -> DummyDB
      -> (DummyDB, Either QueryError Entity)
alter x ent (DummyDB db)
    | isSingular (DummyDB db) x = case select x (DummyDB db) of
        (_, Left  err) -> (DummyDB db, Left err)
        (_, Right old) -> update x (merge ent old) (DummyDB db)
    | isPlural   (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

alterById :: TopLevelKey -> EntityId -> Entity -> DummyDB
          -> (DummyDB, Either QueryError Entity)
alterById x n ent (DummyDB db)
    | isSingular (DummyDB db) x = (DummyDB db, Left KeyTypeMismatch)
    | isPlural   (DummyDB db) x = case selectById (T.pack "id") x n (DummyDB db) of
        (_, Left  err) -> (DummyDB db, Left err)
        (_, Right old) -> updateById x n (merge ent old) (DummyDB db)
    | otherwise                 = (DummyDB db, Left NoSuchEntity)

merge :: Entity -> Entity -> Entity
merge (Object o1) (Object o2) = Object $ HM.union o1 o2
merge ent         _           = ent

mergeById :: EntityId -> Entity -> V.Vector Entity -> V.Vector Entity
mergeById n ent currents =
    case V.findIndex (idIs n) currents of
        Nothing -> currents
        Just idx -> currents V.// [(idx, merged)]
            where
                merged = setId n $ merge ent (currents V.! idx)

idOf :: Identifier -> Entity -> Maybe EntityId
idOf ident ent = ent ^? key ident . _Integer

is :: Identifier -> EntityId -> Entity -> Bool
is ident n ent = idOf ident ent == Just n

idIs :: EntityId -> Entity -> Bool
idIs n ent = idOf (T.pack "id") ent == Just n
