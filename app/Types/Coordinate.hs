module Types.Coordinate (Coordinate, Latitude, Longitude, getLatitude, getLongitude, mkCoordinate) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(Float))
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))
import GHC.Generics (Generic)

data Coordinate = Coordinate
  { latitude :: Latitude
  , longitude :: Longitude
  } deriving (Generic, GQLType)

newtype Latitude = Latitude Double deriving (Eq, Show, Ord)

newtype Longitude = Longitude Double deriving (Eq, Show, Ord)

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
mkLat = normalize (pi / 2) >>> Latitude

mkLong :: Double -> Longitude
mkLong = normalize pi >>> Longitude

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

instance Param Latitude where
  render (Latitude value) = render value

instance Result Latitude where
  convert field = convert field >>> mkLat

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

instance Param Longitude where
  render (Longitude value) = render value

instance Result Longitude where
  convert field = convert field >>> mkLong
