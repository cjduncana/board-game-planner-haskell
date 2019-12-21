module Types.Coordinate (Coordinate, getLatitude, getLongitude) where

import Control.Category ((>>>))
import qualified Data.Text as Text
import Database.SQLite.Simple (SQLData(SQLFloat))
import Database.SQLite.Simple.ToField (ToField(toField))

newtype Latitude = Latitude Double deriving (Eq, Show, Ord)

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
  }

getLatitude :: Coordinate -> Latitude
getLatitude = latitude

getLongitude :: Coordinate -> Longitude
getLongitude = longitude

normalize :: Double -> Double -> Double
normalize upperBound x
    | x > upperBound = normalize upperBound $ x - upperBound
    | x < -upperBound = normalize upperBound $ x + upperBound
    | otherwise = x

mkLat :: Double -> Latitude
mkLat = normalize 90 >>> Latitude

mkLong :: Double -> Longitude
mkLong = normalize 180 >>> Longitude
