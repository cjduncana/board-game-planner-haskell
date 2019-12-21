module Types.User (User, UserTuple, create, encodeJwt, getID, intoTuple) where

import Data.Morpheus.Types (GQLType, ID(unpackID))
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Time.Clock as Clock
import Database.SQLite.Simple (FromRow(fromRow))
import qualified Database.SQLite.Simple as SQLite
import GHC.Generics (Generic)
import Prelude (Maybe(Just), mempty, ($), (*), (<$>), (<*>))
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

data User = User
  { id :: ID
  , name :: NonEmptyText
  , email :: EmailAddress
  } deriving (Generic, GQLType)

data UserTuple = UserTuple User HashedPassword

instance FromRow UserTuple where
  fromRow =
    createUserTuple <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

create :: UUID -> NonEmptyText -> EmailAddress -> User
create uuid name email =
  User
    { id = UUID.toGQLID uuid
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
  let
    claims = mempty
      { iss = JWT.stringOrURI "board-game-planner"
      , sub = JWT.stringOrURI $ unpackID $ id user
      , exp = Time.toNumericDate $ manyDaysLater daysLater now
      , iat = Time.toNumericDate now
      -- TODO: Implement JTI for session invalidation
      }
  in
    JWT.encodeSigned signer header claims

header :: JOSEHeader
header = mempty { alg = Just HS256 }

manyDaysLater :: NominalDiffTime -> Time -> Time
manyDaysLater daysLater = Time.addTime (daysLater * Clock.nominalDay)

getID :: User -> ID
getID = id
