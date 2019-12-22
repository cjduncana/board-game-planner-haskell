module Types.User (User, UserTuple, create, encodeJwt, getID, intoTuple) where

import Control.Category ((>>>))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(String))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Clock
import Database.SQLite.Simple (FromRow(fromRow))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField (ToField(toField))
import GHC.Generics (Generic)
import Prelude
    (Either(Left, Right), Maybe(Just), maybe, mempty, ($), (*), (<$>), (<*>))
import Web.JWT
    ( Algorithm(HS256)
    , JOSEHeader(alg)
    , JWTClaimsSet(exp, iat, iss, sub)
    , Signer
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

data UserTuple = UserTuple User HashedPassword

create :: UUID -> NonEmptyText -> EmailAddress -> User
create uuid name email =
  User
    { id = UserID uuid
    , name = name
    , email = email
    }

createUserTuple :: UUID -> NonEmptyText -> EmailAddress -> HashedPassword -> UserTuple
createUserTuple id name email = UserTuple (create id name email)

intoTuple :: UserTuple -> (User, HashedPassword)
intoTuple (UserTuple user hashedPassword) =
  (user, hashedPassword)

encodeJwt :: NominalDiffTime -> Time -> Signer -> User -> Text
encodeJwt daysLater now signer user =
  JWT.encodeSigned signer header claims
  where
    (UserID uuid) = id user
    claims = mempty
      { iss = JWT.stringOrURI "board-game-planner"
      , sub = JWT.stringOrURI $ UUID.toText uuid
      , exp = Time.toNumericDate $ manyDaysLater daysLater now
      , iat = Time.toNumericDate now
      -- TODO: Implement JTI for session invalidation
      }

header :: JOSEHeader
header = mempty { alg = Just HS256 }

manyDaysLater :: NominalDiffTime -> Time -> Time
manyDaysLater daysLater = Time.addTime (daysLater * Clock.nominalDay)

instance GQLScalar UserID where
  parseValue (String value) =
    maybe (Left "Value should be a UUID") (UserID >>> Right) (UUID.fromText value)
  parseValue _ = Left "Value should be of type String"
  serialize (UserID id) = String $ UUID.toText id

instance GQLType UserID where
  type KIND UserID = SCALAR

instance ToField UserID where
  toField (UserID id) = toField id

instance FromRow UserTuple where
  fromRow =
    createUserTuple <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
