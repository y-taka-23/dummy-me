module Web.DummyMe.DB (
      DummyDB(..)
    , TopLevelKey(..)
    , EntityId(..)
    , loadDummyDB
    , select
    , selectById
    , deleteById
    , insert
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
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
insert key val (DummyDB db) = undefined

appendEntity :: Value -> V.Vector Value -> V.Vector Value
appendEntity val currents = undefined

setId :: EntityId -> Object -> Object
setId n obj = undefined

nextId :: V.Vector Value -> EntityId
nextId currents = undefined

idIs :: EntityId -> Value -> Bool
idIs n val = val ^? key (T.pack "id") . _Integer == Just n
