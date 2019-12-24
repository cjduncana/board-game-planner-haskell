module Types.JWT (JWT, use) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Maybe as Maybe
import Data.Text (Text)
import Polysemy (Embed, Member, Members, Sem)
import qualified Web.JWT as Web

import qualified Effects.User
import Types.Time (Time)
import qualified Types.Time as Time
import Types.User (User)
import qualified Types.User as User

type JWT = Web.JWT Web.VerifiedJWT

data JWTError
  = Missing
  | Expired
  | Invalid

instance Show JWTError where
  show Missing = "Missing credentials"
  show Expired = "Expired credentials"
  show Invalid = "Invalid credentials"

use ::
  Members [Embed IO, Effects.User.User] r
  => Web.Signer
  -> Text
  -> (User -> Sem r (Either String a))
  -> Sem r (Either String a)
use signer encodedToken fn = do
  now <- Time.getNow
  case decodeAndVerify signer now encodedToken of
    Left e -> pure $ Left $ show e
    Right jwt ->
      findUser jwt
        >>= maybe (pure $ Left $ show Invalid) fn

decodeAndVerify :: Web.Signer -> Time -> Text -> Either JWTError JWT
decodeAndVerify signer now encodedToken = do
  decodedToken <- maybe (Left Missing) Right (Web.decode encodedToken)
  if hasExpired now decodedToken
    then
      Left Expired
    else
      maybe (Left Invalid) Right (Web.verify signer decodedToken)

findUser :: Member Effects.User.User r => JWT -> Sem r (Maybe User)
findUser jwt =
  case User.getUserIDFromJWT jwt of
    Nothing -> pure Nothing
    Just userID -> Effects.User.findOne userID

hasExpired :: Time -> Web.JWT r -> Bool
hasExpired now =
  Web.claims
    >>> Web.exp
    >>> (=<<) beforeToday
    >>> Maybe.fromMaybe True

  where
    beforeToday :: Web.NumericDate -> Maybe Bool
    beforeToday expDate =
      Time.toNumericDate now
        & fmap (> expDate)
