module Types.EventID (EventID, new) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Database.SQLite.Simple.FromField (FromField(fromField))
import Database.SQLite.Simple.ToField (ToField(toField))
import Polysemy (Embed, Member, Sem)
import Prelude
    (Either(Left, Right), Eq, IO, Ord(compare), Show(show), ($), (<$>), (==))
import qualified Prelude

import Types.UUID (UUID)
import qualified Types.UUID as UUID

newtype EventID = EventID UUID

new :: Member (Embed IO) r => Sem r EventID
new = EventID <$> UUID.randomUUID

instance Eq EventID where
  (EventID id1) == (EventID id2) = id1 == id2

instance FromField EventID where
  fromField field = EventID <$> fromField field

instance GQLScalar EventID where
  parseValue (String value) =
    UUID.fromText value
      & Prelude.maybe (Left "Value should be a UUID") (EventID >>> Right)
  parseValue _ = Left "Value should be of type String"
  serialize (EventID id) = String $ UUID.toText id

instance GQLType EventID where
  type KIND EventID = SCALAR

instance Ord EventID where
  compare (EventID id1) (EventID id2) = compare id1 id2

instance Show EventID where
  show (EventID id) = show id

instance ToField EventID where
  toField (EventID id) = toField id
