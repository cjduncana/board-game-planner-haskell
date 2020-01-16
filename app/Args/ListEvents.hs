module Args.ListEvents
  ( ByLocation
  , ListEventsArgs
  , byGameIDs
  , byLocation
  , byPlayerIDs
  , distanceInKm
  , latitude
  , longitude
  , startAfter
  , token
  ) where

import Data.Morpheus.Kind (INPUT)
import Data.Morpheus.Types (GQLType(description), KIND)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types.BoardGame (BoardGameID)
import Types.Coordinate (Latitude, Longitude)
import Types.Time (Time)
import Types.User (UserID)

-- TODO: Replace [a] with NonEmpty a
-- https://github.com/morpheusgraphql/morpheus-graphql/issues/341
data ListEventsArgs = ListEventsArgs
  { token :: Text
  , startAfter :: Time
  , byGameIDs :: Maybe [BoardGameID]
  , byPlayerIDs :: Maybe [UserID]
  , byLocation :: Maybe ByLocation
  } deriving (Generic)

data ByLocation = ByLocation
  { latitude :: Latitude
  , longitude :: Longitude
  , distanceInKm :: Int
  } deriving (Generic)

instance GQLType ByLocation where
  type KIND ByLocation = INPUT
  description _ = Just "List Events near a Location"
