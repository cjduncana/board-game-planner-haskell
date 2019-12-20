module Types.Event (Event, create) where

import Data.Morpheus.Types (GQLType, ID(ID))
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Prelude ()

import Types.BoardGame (BoardGame)
import Types.Coordinate (Coordinate)
import Types.User (User)
import Types.UUID (UUID)
import qualified Types.UUID as UUID

data Event = Event
  { id :: ID
  , creator :: User
  , startTime :: UTCTime
  , location :: Coordinate
  , players :: [User]
  , games :: [BoardGame]
  } deriving (Generic, GQLType)

create :: UUID -> User -> UTCTime -> Coordinate -> [BoardGame] -> Event
create id creator startTime location games = Event
  { id = UUID.toGQLID id
  , creator = creator
  , startTime = startTime
  , location = location
  , players = [creator]
  , games = games
  }
