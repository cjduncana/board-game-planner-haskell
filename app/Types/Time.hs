module Types.Time (Time, addTime, getNow, toNumericDate) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import qualified Data.Text as Text
import Data.Time.Calendar (Day(ModifiedJulianDay, toModifiedJulianDay))
import Data.Time.Clock (NominalDiffTime, UTCTime(UTCTime, utctDay, utctDayTime))
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format.ISO8601 as TimeFormat
import Database.SQLite.Simple.ToField (ToField(toField))
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Web.JWT (NumericDate)
import qualified Web.JWT as JWT

newtype Time = Time UTCTime

toNumericDate :: Time -> Maybe NumericDate
toNumericDate (Time time) =
  Time.diffUTCTime time epoch
    & JWT.numericDate

addTime :: NominalDiffTime -> Time -> Time
addTime diff (Time time) =
  Time (Time.addUTCTime diff time)

epoch :: UTCTime
epoch = UTCTime
  { utctDay = ModifiedJulianDay {toModifiedJulianDay = 40587}
  , utctDayTime = Time.secondsToDiffTime 0
  }

getNow :: Member (Embed IO) r => Sem r Time
getNow =
  Time <$> Polysemy.embed Time.getCurrentTime

instance GQLScalar Time where
  parseValue (String value) =
    Text.unpack value
      & TimeFormat.iso8601ParseM
      & maybe (Left "String should be in ISO8601 format") (Time >>> Right)
  parseValue _ = Left "Value should be of type String"
  serialize (Time time) =
    TimeFormat.iso8601Show time
      & Text.pack
      & String

instance GQLType Time where
  type KIND Time = SCALAR

instance ToField Time where
  toField (Time time) = toField time
