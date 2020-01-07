module Types.UUID (UUID, fromText, randomUUID, toText) where

import Control.Exception.Base (Exception)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Prelude (Eq, IO, Maybe, Ord(compare), Show(show), (<$>), (==))
import qualified Prelude

import qualified Types.Utils as Utils

newtype UUID = UUID UUID.UUID

data UUIDException
  = NotAUUID
  | NotAText
  deriving (Exception, Show)

fromText :: Text -> Maybe UUID
fromText text = UUID <$> UUID.fromText text

randomUUID :: Member (Embed IO) r => Sem r UUID
randomUUID =
  UUID <$> Polysemy.embed UUID.nextRandom

toText :: UUID -> Text
toText (UUID id) =
  UUID.toText id

instance Eq UUID where
  (UUID id1) == (UUID id2) = id1 == id2

instance Ord UUID where
  compare (UUID id1) (UUID id2) = compare id1 id2

instance Param UUID where
  render (UUID id) = render (UUID.toString id)

instance Result UUID where
  convert field value =
    Prelude.maybe
      (Utils.conversionFailed field "UUID" "could not parse")
      UUID
      (UUID.fromString (convert field value))

instance Show UUID where
  show (UUID id) = show id
