module Types.UTC (toNumericDate) where

import Control.Category ((>>>))
import Data.Time.Calendar (Day(ModifiedJulianDay, toModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime, utctDay, utctDayTime))
import qualified Data.Time.Clock as Time
import Web.JWT (NumericDate)
import qualified Web.JWT as JWT

toNumericDate :: UTCTime -> Maybe NumericDate
toNumericDate =
  Time.diffUTCTime epoch >>> JWT.numericDate

epoch :: UTCTime
epoch = UTCTime
  { utctDay = ModifiedJulianDay {toModifiedJulianDay = 40587}
  , utctDayTime = Time.secondsToDiffTime 0
  }
