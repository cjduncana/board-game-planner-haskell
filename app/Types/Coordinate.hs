module Types.Coordinate (Coordinate, Latitude, Longitude, getLatitude, getLongitude, mkCoordinate) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(Float))
import Database.SQLite.Simple (SQLData(SQLFloat))
import Database.SQLite.Simple.ToField (ToField(toField))
import GHC.Generics (Generic)

newtype Latitude = Latitude Double deriving (Eq, Show, Ord)

instance GQLScalar Latitude where
  parseValue (Float value) =
    realToFrac value
      & mkLat
      & Right
  parseValue _ = Left "Value should be of type Float"
  serialize (Latitude lat) =
    realToFrac lat
      & Float

instance GQLType Latitude where
  type KIND Latitude = SCALAR

instance Num Latitude where
    (Latitude x) + (Latitude y) = mkLat $ x + y
    (Latitude x) - (Latitude y) = mkLat $ x - y
    (Latitude x) * (Latitude y) = mkLat $ x * y
    negate (Latitude x) = Latitude $ negate x
    abs (Latitude x) = Latitude $ abs x
    signum (Latitude x) = Latitude $ signum x
    fromInteger = fromInteger >>> mkLat

instance ToField Latitude where
  toField (Latitude value) = SQLFloat value

newtype Longitude = Longitude Double deriving (Eq, Show, Ord)

instance GQLScalar Longitude where
  parseValue (Float value) =
    realToFrac value
      & mkLong
      & Right
  parseValue _ = Left "Value should be of type Float"
  serialize (Longitude long) =
    realToFrac long
      & Float

instance GQLType Longitude where
  type KIND Longitude = SCALAR

instance Num Longitude where
    (Longitude x) + (Longitude y) = mkLong $ x + y
    (Longitude x) - (Longitude y) = mkLong $ x - y
    (Longitude x) * (Longitude y) = mkLong $ x * y
    negate (Longitude x) = Longitude $ negate x
    abs (Longitude x) = Longitude $ abs x
    signum (Longitude x) = Longitude $ signum x
    fromInteger = fromInteger >>> mkLong

instance ToField Longitude where
  toField (Longitude value) = SQLFloat value

data Coordinate = Coordinate
  { latitude :: Latitude
  , longitude :: Longitude
  } deriving (Generic, GQLType)

getLatitude :: Coordinate -> Latitude
getLatitude = latitude

getLongitude :: Coordinate -> Longitude
getLongitude = longitude

normalize :: Double -> Double -> Double
normalize upperBound x
    | x > upperBound = normalize upperBound $ x - upperBound
    | x < -upperBound = normalize upperBound $ x + upperBound
    | otherwise = x

mkCoordinate :: Latitude -> Longitude -> Coordinate
mkCoordinate = Coordinate

mkLat :: Double -> Latitude
mkLat = normalize 90 >>> Latitude

mkLong :: Double -> Longitude
mkLong = normalize 180 >>> Longitude
