module Args.ListEvents
  ( ByLocationArgs
  , ListEventsArgs
  , byGameIDs
  , byLocationArgs
  , byPlayerIDs
  , startAfter
  ) where

import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType, KIND)
import GHC.Generics (Generic)

import Types.BoardGame (BoardGameID)
import Types.Coordinate (Latitude, Longitude)
import Types.Time (Time)
import Types.User (UserID)

-- TODO: Replace [a] with NonEmpty a
-- https://github.com/morpheusgraphql/morpheus-graphql/issues/341
data ListEventsArgs = ListEventsArgs
  { startAfter :: Time
  , byGameIDs :: Maybe [BoardGameID]
  , byPlayerIDs :: Maybe [UserID]
  , byLocationArgs :: Maybe ByLocationArgs
  } deriving (Generic)

data ByLocationArgs = ByLocationArgs
  { latitude :: Latitude
  , longitude :: Longitude
  , distanceInKm :: Int
  } deriving (Generic)

instance GQLType ByLocationArgs where
  type KIND ByLocationArgs = INPUT
