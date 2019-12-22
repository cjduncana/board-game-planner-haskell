module Types.Event (Event, create) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import GHC.Generics (Generic)
import Prelude (Either(Left, Right), maybe, ($))

import Types.BoardGame (BoardGame)
import Types.Coordinate (Coordinate)
import Types.Time (Time)
import Types.User (User)
import Types.UUID (UUID)
import qualified Types.UUID as UUID

newtype EventID = EventID UUID

data Event = Event
  { id :: EventID
  , creator :: User
  , startTime :: Time
  , location :: Coordinate
  , players :: [User]
  , games :: [BoardGame]
  } deriving (Generic, GQLType)

create :: UUID -> User -> Time -> Coordinate -> [BoardGame] -> Event
create id creator startTime location games = Event
  { id = EventID id
  , creator = creator
  , startTime = startTime
  , location = location
  , players = [creator]
  , games = games
  }

instance GQLScalar EventID where
  parseValue (String value) =
    UUID.fromText value
      & maybe (Left "Value should be a UUID") (EventID >>> Right)
  parseValue _ = Left "Value should be of type String"
  serialize (EventID id) = String $ UUID.toText id

instance GQLType EventID where
  type KIND EventID = SCALAR
