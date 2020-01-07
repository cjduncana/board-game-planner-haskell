module Resolver.User
  ( CreateUserArgs
  , resolveCreateUser
  ) where

import Data.Function ((&))
import Data.Morpheus.Types (MutRes)
import qualified Data.Morpheus.Types as M
import Database.MySQL.Simple (Connection)
import GHC.Generics (Generic)
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Input as Input

import Effects.CryptoHash (CryptoHash)
import qualified Effects.CryptoHash as CryptoHash
import qualified Effects.User as Effects
import Types.EmailAddress (EmailAddress)
import Types.NonEmptyText (NonEmptyText)
import Types.Password (Password)
import qualified Types.User as Types

data CreateUserArgs = CreateUserArgs
  { name :: NonEmptyText
  , email :: EmailAddress
  , password :: Password
  } deriving (Generic)

resolveCreateUser :: Connection -> CreateUserArgs -> MutRes () IO Types.User
resolveCreateUser conn CreateUserArgs {name, email, password} =
  M.liftEither $
    createUser name email password
      & CryptoHash.runCryptoHashAsArgon2
      & Effects.runUserAsMySQL
      & Input.runInputConst conn
      & Polysemy.runM

createUser ::
  Members [CryptoHash, Effects.User, Embed IO] r
  => NonEmptyText
  -> EmailAddress
  -> Password
  -> Sem r (Either String Types.User)
createUser name email password = do
  hashedPasswordResult <- CryptoHash.makeHash password
  either
    error
    (\hashedPassword -> do
      userResult <- Effects.create name email hashedPassword
      either
        (\_ -> pure $ Left "An error occured while creating the user")
        (pure . Right)
        userResult
    )
    hashedPasswordResult
