module Types.UUID (UUID, fromText, randomUUID, toText) where

import Control.Category ((>>>))
import Control.Exception.Base (Exception, SomeException(SomeException))
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField (FromField(fromField), fieldData)
import Database.SQLite.Simple.Ok (Ok(Errors, Ok))
import Database.SQLite.Simple.ToField (ToField(toField))
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Prelude (IO, Maybe, Show, maybe, (<$>))

newtype UUID = UUID UUID.UUID

data UUIDException
  = NotAUUID
  | NotAText
  deriving (Exception, Show)

instance FromField UUID where
  fromField field =
    case fieldData field of
      SQLText text -> maybe (Errors [SomeException NotAUUID]) (UUID >>> Ok) (UUID.fromText text)
      _ -> Errors [SomeException NotAText]

instance ToField UUID where
  toField (UUID uuid) = (UUID.toText >>> SQLText) uuid

fromText :: Text -> Maybe UUID
fromText text = UUID <$> UUID.fromText text

randomUUID :: Member (Embed IO) r => Sem r UUID
randomUUID =
  UUID <$> Polysemy.embed UUID.nextRandom

toText :: UUID -> Text
toText (UUID id) =
  UUID.toText id
