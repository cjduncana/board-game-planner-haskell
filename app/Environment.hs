module Environment
  ( Environment
  , expireDaysLater
  , getEnvironment
  , jwtSigner
  , sqliteDatabaseName
  ) where

import Control.Category ((>>>))
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Clock
import qualified System.Environment as Env
import qualified Text.Read as Read
import Web.JWT (Signer)
import qualified Web.JWT as JWT

data Environment = Environment
  { jwtSigner :: Signer
  , expireDaysLater :: NominalDiffTime
  , sqliteDatabaseName :: String
  }

getEnvironment :: IO Environment
getEnvironment =
  Environment
    <$> (getJWTSigner <$> Env.lookupEnv "JWT_SIGNER")
    <*> (getExpireDaysLater <$> Env.lookupEnv "EXPIRE_DAYS_LATER")
    <*> (Maybe.fromMaybe "sqlite.db" <$> Env.lookupEnv "SQLITE_DATABASE_NAME")

getJWTSigner :: Maybe String -> Signer
getJWTSigner = maybe (JWT.hmacSecret "Secret") (Text.pack >>> JWT.hmacSecret)

getExpireDaysLater :: Maybe String -> NominalDiffTime
getExpireDaysLater = (parseExpireDaysLater =<<) >>> Maybe.fromMaybe (7 * Clock.nominalDay)

parseExpireDaysLater :: String -> Maybe NominalDiffTime
parseExpireDaysLater = Read.readMaybe >>> fmap (fromInteger >>> (*) Clock.nominalDay)
