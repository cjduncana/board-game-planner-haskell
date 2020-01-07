module Types.BoardGame (BoardGame, BoardGameID, getID) where

import Control.Category ((>>>))
import qualified Data.Map.Lazy as Map
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types
    (GQLScalar(parseValue, serialize), GQLType, KIND, ScalarValue(Int))
import Database.MySQL.Simple.Param (Param(render))
import Database.MySQL.Simple.Result (Result(convert))
import GHC.Generics (Generic)
import Prelude
    ( Bool(False)
    , Either(Left, Right)
    , Eq
    , Int
    , Ord(compare)
    , Show(show)
    , ($)
    , (&&)
    , (<$>)
    , (<*>)
    , (==)
    )
import qualified Prelude
import Text.XML (Element(Element), Node(NodeElement))
import Text.XML.Decode.DecodeCursor (DecodeCursor)
import qualified Text.XML.Decode.DecodeCursor as DecodeCursor
import Text.XML.Decode.HCursor (Predicate(Predicate, _predDesc, _predFun), (%/))
import qualified Text.XML.Decode.HCursor as HCursor
import qualified Text.XML.Decode.Parsers as Parsers

import Types.NonEmptyText (NonEmptyText)
import qualified Types.NonEmptyText as NonEmptyText
import Types.PositiveInteger (PositiveInteger)
import qualified Types.PositiveInteger as PositiveInteger

newtype BoardGameID = BoardGameID Int

data BoardGame = BoardGame
  { id :: BoardGameID
  , name :: NonEmptyText
  , description :: NonEmptyText
  , minimumAmountOfPlayers :: PositiveInteger
  , maximumAmountOfPlayers :: PositiveInteger
  , thumbnailUrl :: NonEmptyText
  , imageUrl :: NonEmptyText
  } deriving (Generic, GQLType, Show)

getID :: BoardGame -> BoardGameID
getID = id

instance DecodeCursor BoardGame where
  decode cursor =
    BoardGame
      <$> (BoardGameID <$> DecodeCursor.decodeAttr "id" Parsers.parseInt cursor)
      <*> DecodeCursor.decodeAttr "value" NonEmptyText.fromText (cursor %/ HCursor.filterPred namePredicate)
      <*> DecodeCursor.decodeSingle  (cursor %/ HCursor.laxElement "description")
      <*> DecodeCursor.decodeAttr "value" PositiveInteger.fromText (cursor %/ HCursor.laxElement "minplayers")
      <*> DecodeCursor.decodeAttr "value" PositiveInteger.fromText (cursor %/ HCursor.laxElement "maxplayers")
      <*> DecodeCursor.decodeSingle  (cursor %/ HCursor.laxElement "thumbnail")
      <*> DecodeCursor.decodeSingle  (cursor %/ HCursor.laxElement "image")

namePredicate :: Predicate
namePredicate =
  Predicate
    { _predDesc = "Name"
    , _predFun = \case
        NodeElement element ->
          isNameElement && hasPrimaryType
          where
            Element elementName elementAttributes _ = element
            isNameElement = elementName == "name"
            maybeAttr = Map.lookup "type" elementAttributes
            hasPrimaryType = Prelude.maybe False ("primary" ==) maybeAttr
        _ -> False
    }

instance DecodeCursor BoardGameID where
  decode cursor =
    BoardGameID <$>
      DecodeCursor.decodeAttr "id" Parsers.parseInt cursor

instance Eq BoardGameID where
  (BoardGameID id1) == (BoardGameID id2) = id1 == id2

instance GQLScalar BoardGameID where
  parseValue (Int value) = Right $ BoardGameID value
  parseValue _ = Left "Value should be of type Int"
  serialize (BoardGameID id) = Int id

instance GQLType BoardGameID where
  type KIND BoardGameID = SCALAR

instance Ord BoardGameID where
  compare (BoardGameID id1) (BoardGameID id2) =
    compare id1 id2

instance Param BoardGameID where
  render (BoardGameID id) = render id

instance Result BoardGameID where
  convert field = convert field >>> BoardGameID

instance Show BoardGameID where
  show (BoardGameID id) = show id
