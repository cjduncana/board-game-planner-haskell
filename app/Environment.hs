module Environment
  ( Environment
  , connectInfo
  , expireDaysLater
  , getEnvironment
  , jwtSigner
  ) where

import Control.Category ((>>>))
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Clock
import Database.MySQL.Simple
    (ConnectInfo(connectDatabase, connectHost, connectPassword, connectUser))
import qualified Database.MySQL.Simple as MySQL
import qualified System.Environment as Env
import qualified Text.Read as Read
import Web.JWT (Signer)
import qualified Web.JWT as JWT

data Environment = Environment
  { jwtSigner :: Signer
  , expireDaysLater :: NominalDiffTime
  , connectInfo :: ConnectInfo
  }

getEnvironment :: IO Environment
getEnvironment =
  Environment
    <$> (getJWTSigner <$> Env.lookupEnv "JWT_SIGNER")
    <*> (getExpireDaysLater <$> Env.lookupEnv "EXPIRE_DAYS_LATER")
    <*> getConnectionInfo

getJWTSigner :: Maybe String -> Signer
getJWTSigner = maybe (JWT.hmacSecret "Secret") (Text.pack >>> JWT.hmacSecret)

getExpireDaysLater :: Maybe String -> NominalDiffTime
getExpireDaysLater = (parseExpireDaysLater =<<) >>> Maybe.fromMaybe (7 * Clock.nominalDay)

parseExpireDaysLater :: String -> Maybe NominalDiffTime
parseExpireDaysLater = Read.readMaybe >>> fmap (fromInteger >>> (*) Clock.nominalDay)

getConnectionInfo :: IO ConnectInfo
getConnectionInfo =
  mkConnectionInfo
    <$> Env.lookupEnv "DB_HOST"
    <*> Env.lookupEnv "DB_USER"
    <*> Env.lookupEnv "DB_PASSWORD"
    <*> Env.lookupEnv "DB_NAME"

mkConnectionInfo ::
  Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> ConnectInfo
mkConnectionInfo host user password database =
  MySQL.defaultConnectInfo
    { connectHost = Maybe.fromMaybe "mysql" host
    , connectUser = Maybe.fromMaybe "root" user
    , connectPassword = Maybe.fromMaybe "board-game-planner" password
    , connectDatabase = Maybe.fromMaybe "board_game_planner" database
    }
