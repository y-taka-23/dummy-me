module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , loadDummyDB
    , select
    , selectById
    , deleteById
    , insert
    , update
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

instance Eq DummyDB where
    (==) (DummyDB x) (DummyDB y) =
        (decode x :: Maybe Value) == (decode y :: Maybe Value)

loadDummyDB :: FilePath -> IO DummyDB
loadDummyDB fp = DummyDB <$> BS.readFile fp

select :: TopLevelKey -> DummyDB -> (DummyDB, Maybe Value)
select x (DummyDB db) = (DummyDB db, db ^? key x)

-- TODO: It doen't go when there are multiple entities of the specified id
selectById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
selectById x n (DummyDB db) =
    (DummyDB db, db ^? key x . _Array . traverse . filtered (idIs n))

deleteById :: TopLevelKey -> EntityId -> DummyDB -> (DummyDB, Maybe Value)
deleteById x n (DummyDB db) =
    let (_, mDeletedEntity) = selectById x n (DummyDB db)
    in  (DummyDB $ db & key x . _Array %~ purgeEntity n, mDeletedEntity)

purgeEntity :: EntityId -> V.Vector Value -> V.Vector Value
purgeEntity n = V.filter (not . idIs n)

insert :: TopLevelKey -> Value -> DummyDB -> (DummyDB, Maybe Value)
insert x val (DummyDB db) =
    case nextId <$> db ^? key x . _Array of
        Nothing -> (DummyDB db, Nothing)
        Just newId ->
            let newVal = val & _Object %~ setId newId
            in  ( DummyDB $ db & key x . _Array %~ appendEntity newVal
                , Just newVal
                )

appendEntity :: Value -> V.Vector Value -> V.Vector Value
appendEntity = flip V.snoc

setId :: EntityId -> Object -> Object
setId n = HM.insert (T.pack "id") (toJSON n)

nextId :: V.Vector Value -> EntityId
nextId currents =
    case maximumOf (traverse . key (T.pack "id") . _Number) currents of
        Nothing -> 1
        Just sci -> case SCI.floatingOrInteger sci of
            Left _  -> 1
            Right n -> n + 1

update :: TopLevelKey -> Value -> DummyDB -> (DummyDB, Maybe Value)
update = undefined

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key (T.pack "id") . _Integer == Just n
