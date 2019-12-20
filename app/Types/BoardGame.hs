module Types.BoardGame (BoardGame) where

import qualified Data.Map.Lazy as Map
import Data.Morpheus.Types (GQLType, ID(ID))
import GHC.Generics (Generic)
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

data BoardGame = BoardGame
  { id :: ID
  , name :: NonEmptyText
  , description :: NonEmptyText
  , minimumAmountOfPlayers :: PositiveInteger
  , maximumAmountOfPlayers :: PositiveInteger
  , thumbnailUrl :: NonEmptyText
  , imageUrl :: NonEmptyText
  } deriving (Generic, GQLType)

instance DecodeCursor BoardGame where
  decode cursor =
    BoardGame
      <$> (ID <$> DecodeCursor.decodeAttr "id" Parsers.parseText cursor)
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
            hasPrimaryType = maybe False ("primary" ==) maybeAttr
        _ -> False
    }
