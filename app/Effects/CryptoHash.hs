module Effects.CryptoHash
  ( CryptoHash
  , makeHash
  , runCryptoHashAsArgon2
  , validateHash
  ) where

import Crypto.Argon2 (Argon2Status(Argon2Ok))
import qualified Crypto.RNG as RNG
import Polysemy (Embed, Member, Sem)
import qualified Polysemy

import Types.HashedPassword (HashedPassword)
import qualified Types.HashedPassword as HashedPassword
import Types.Password (Password)

data CryptoHash m a where
  -- Generates a hash from a password
  MakeHash :: Password -> CryptoHash m (Either String HashedPassword)
  -- Check if a password matches a hash
  ValidateHash :: HashedPassword -> Password -> CryptoHash m Bool

Polysemy.makeSem ''CryptoHash

runCryptoHashAsArgon2 :: Member (Embed IO) r => Sem (CryptoHash : r) a -> Sem r a
runCryptoHashAsArgon2 = Polysemy.interpret $ \case

    MakeHash password -> do
      state <- Polysemy.embed RNG.newCryptoRNGState
      salt <- Polysemy.embed $ RNG.randomBytesIO saltLengthInBytes state
      pure $ HashedPassword.encodePassword password salt

    ValidateHash hashedPassword password -> pure $
      case HashedPassword.verifyPassword hashedPassword password of
        Argon2Ok -> True
        _ -> False

-- 20 characters
saltLengthInBytes :: Int
saltLengthInBytes = 640
