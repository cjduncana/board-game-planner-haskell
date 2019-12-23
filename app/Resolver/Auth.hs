module Resolver.Auth
  ( TokenArgs
  , resolveToken
  ) where

import Data.Function ((&))
import Data.Morpheus.Types (Res)
import qualified Data.Morpheus.Types as M
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
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
import qualified Types.Time as Time
import qualified Types.User as User

data TokenArgs = TokenArgs
  { email :: EmailAddress
  , password :: Password
  } deriving (Generic)

resolveToken :: Connection -> NominalDiffTime -> Signer -> TokenArgs -> Res () IO Text
resolveToken conn daysLater signer TokenArgs {email, password} =
  M.liftEither $
    token daysLater email password
      & CryptoHash.runCryptoHashAsArgon2
      & State.evalState signer
      & User.runUserAsSQLite
      & Input.runInputConst conn
      & Polysemy.runM

token ::
  Members [CryptoHash, State Signer, User, Embed IO] r
  => NominalDiffTime
  -> EmailAddress
  -> Password
  -> Sem r (Either String Text)
token daysLater email password = do
  maybeUserTuple <- User.findByEmailAddress email
  case maybeUserTuple of
    Nothing -> pure $ Left tokenErrorMessage
    Just (user, hashedPassword) -> do
      isValid <- CryptoHash.validateHash hashedPassword password
      if not isValid
        then pure $ Left tokenErrorMessage
        else do
          now <- Time.getNow
          signer <- State.get
          pure $ Right $ User.encodeJwt daysLater now signer user

tokenErrorMessage :: String
tokenErrorMessage = "Invalid user ID or password"
