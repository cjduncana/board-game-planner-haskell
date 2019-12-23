module Types.User
  ( User
  , UserID
  , encodeJwt
  , findOne
  , getID
  , getUserIDFromJWT
  , newUser
  ) where

import Control.Category ((>>>))
import qualified Data.Maybe as Maybe
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Clock
import Database.SQLite.Simple (Connection, NamedParam, Query)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField(fromField))
import Database.SQLite.Simple.ToField (ToField(toField))
import GHC.Generics (Generic)
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Prelude
    (Either(Left, Right), IO, Maybe(Just), ($), (*), (<$>), (<*>), (=<<))
import qualified Prelude
import Web.JWT
    ( Algorithm(HS256)
    , JOSEHeader(alg)
    , JWT
    , JWTClaimsSet(exp, iat, iss, sub)
    , Signer
    , VerifiedJWT
    )
import qualified Web.JWT as JWT

import Types.EmailAddress (EmailAddress)
import Types.HashedPassword (HashedPassword)
import Types.NonEmptyText (NonEmptyText)
import Types.Time (Time)
import qualified Types.Time as Time
import Types.UUID (UUID)
import qualified Types.UUID as UUID

newtype UserID = UserID UUID

data User = User
  { id :: UserID
  , name :: NonEmptyText
  , email :: EmailAddress
  } deriving (Generic, GQLType)

getID :: User -> UserID
getID = id

newUser :: Member (Embed IO) r => NonEmptyText -> EmailAddress -> Sem r User
newUser name email =
  User
    <$> (UserID <$> UUID.randomUUID)
    <*> Prelude.pure name
    <*> Prelude.pure email

findOne :: Member (Embed IO) r => Connection -> Query -> [NamedParam] -> Sem r (Maybe (User, HashedPassword))
findOne conn query params =
  Polysemy.embed $ do
      results <- SQLite.queryNamed conn query params
      Prelude.pure (createUserTuple <$> Maybe.listToMaybe results)

createUserTuple :: (UserID, NonEmptyText, EmailAddress, HashedPassword) -> (User, HashedPassword)
createUserTuple (id, name, email, hashedPassword) = (User id name email, hashedPassword)

encodeJwt :: NominalDiffTime -> Time -> Signer -> User -> Text
encodeJwt daysLater now signer user =
  JWT.encodeSigned signer header claims
  where
    (UserID uuid) = id user
    claims = Prelude.mempty
      { iss = JWT.stringOrURI "board-game-planner"
      , sub = JWT.stringOrURI $ UUID.toText uuid
      , exp = Time.toNumericDate $ manyDaysLater daysLater now
      , iat = Time.toNumericDate now
      -- TODO: Implement JTI for session invalidation
      }

getUserIDFromJWT :: JWT VerifiedJWT -> Maybe UserID
getUserIDFromJWT =
  JWT.claims
    >>> JWT.sub
    >>> (<$>) JWT.stringOrURIToText
    >>> (=<<) UUID.fromText
    >>> (<$>) UserID

header :: JOSEHeader
header = Prelude.mempty { alg = Just HS256 }

manyDaysLater :: NominalDiffTime -> Time -> Time
manyDaysLater daysLater = Time.addTime (daysLater * Clock.nominalDay)

instance FromField UserID where
  fromField field = UserID <$> fromField field

instance GQLScalar UserID where
  parseValue (String value) =
    Prelude.maybe
      (Left "Value should be a UUID")
      (UserID >>> Right)
      (UUID.fromText value)
  parseValue _ = Left "Value should be of type String"
  serialize (UserID id) = String $ UUID.toText id

instance GQLType UserID where
  type KIND UserID = SCALAR

instance ToField UserID where
  toField (UserID id) = toField id
