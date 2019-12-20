module Resolver.Auth
  ( TokenArgs
  , resolveToken
  ) where

import Data.Function ((&))
import Data.Morpheus.Types (Res)
import qualified Data.Morpheus.Types as M
import Data.Text (Text)
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Input as Input
import Polysemy.State (State)
import qualified Polysemy.State as State
import Web.JWT (Signer)

import Effects.CryptoHash (CryptoHash)
import qualified Effects.CryptoHash as CryptoHash
import Effects.User (User)
import qualified Effects.User as User
import Types.EmailAddress (EmailAddress)
import Types.Password (Password)
import qualified Types.User as User

data TokenArgs = TokenArgs
  { email :: EmailAddress
  , password :: Password
  } deriving (Generic)

resolveToken :: Connection -> Signer -> TokenArgs -> Res () IO Text
resolveToken conn signer TokenArgs {email, password} =
  M.liftEither $
    token email password
      & CryptoHash.runCryptoHashAsArgon2
      & State.evalState signer
      & User.runUserAsSQLite
      & Input.runInputConst conn
      & Polysemy.runM

token ::
  Members [CryptoHash, State Signer, User, Embed IO] r
  => EmailAddress
  -> Password
  -> Sem r (Either String Text)
token email password = do
  maybeUserTuple <- User.findByEmailAddress email
  case User.intoTuple <$> maybeUserTuple of
    Nothing -> pure $ Left tokenErrorMessage
    Just (user, hashedPassword) -> do
      isValid <- CryptoHash.validateHash hashedPassword password
      if not isValid
        then pure $ Left tokenErrorMessage
        else do
          now <- Polysemy.embed Time.getCurrentTime
          signer <- State.get
          pure $ Right $ User.encodeJwt now signer user

tokenErrorMessage :: String
tokenErrorMessage = "Invalid user ID or password"
