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
import Prelude (Eq, IO, Maybe, Ord(compare), Show(show), (<$>), (==))
import qualified Prelude

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

instance FromField UUID where
  fromField field =
    case fieldData field of
      SQLText text ->
        Prelude.maybe
          (Errors [SomeException NotAUUID])
          (UUID >>> Ok)
          (UUID.fromText text)
      _ -> Errors [SomeException NotAText]

instance Ord UUID where
  compare (UUID id1) (UUID id2) = compare id1 id2

instance Show UUID where
  show (UUID id) = show id

instance ToField UUID where
  toField (UUID uuid) = (UUID.toText >>> SQLText) uuid
