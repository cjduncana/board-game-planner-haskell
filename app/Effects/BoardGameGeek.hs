module Effects.BoardGameGeek
  ( BoardGameGeek
  , getBoardGames
  , runBoardGameGeek
  , searchBoardGameIDsByQuery
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy.Encoding as LazyText
import Network.HTTP.Client (Manager, Request(host, path, port, secure))
import qualified Network.HTTP.Client as HTTP
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import Polysemy.State (State)
import qualified Polysemy.State as State
import Prelude
    ( Bool(True)
    , IO
    , Maybe(Just)
    , Show(show)
    , either
    , error
    , pure
    , show
    , ($)
    , (<$>)
    )
import qualified Text.XML as XML
import Text.XML.Decode.DecodeCursor (DecodeCursor, DecodeResult)
import qualified Text.XML.Decode.DecodeCursor as DecodeCursor
import Text.XML.Decode.HCursor (HCursor, (%/))
import qualified Text.XML.Decode.HCursor as HCursor

import Types.BoardGame (BoardGame, BoardGameID)
import Types.NonEmptyText (NonEmptyText)
import qualified Types.NonEmptyText as NonEmptyText

data BoardGameGeek m a where
  -- Search for board game IDs by a query
  SearchBoardGameIDsByQuery :: NonEmptyText -> BoardGameGeek m [BoardGameID]
  -- Search for board games given a list of IDs
  GetBoardGames :: [BoardGameID] -> BoardGameGeek m [BoardGame]

Polysemy.makeSem ''BoardGameGeek

runBoardGameGeek :: Members [State Manager, Embed IO] r => Sem (BoardGameGeek : r) a -> Sem r a
runBoardGameGeek = Polysemy.interpret $ \case

  SearchBoardGameIDsByQuery query -> do
    manager <- State.get
    response <- Polysemy.embed $ HTTP.httpLbs (searchBoardGamesRequest query) manager
    decodeBody $ HTTP.responseBody response

  GetBoardGames boardGameIDs -> do
    manager <- State.get
    response <- Polysemy.embed $ HTTP.httpLbs (getBoardGamesRequest boardGameIDs) manager
    decodeBody $ HTTP.responseBody response

decodeBody :: DecodeCursor a => ByteString -> Sem r [a]
decodeBody body =
  either (show >>> error) pure (decodeInManyBoardGames cursor)
  where
    cursor =
      LazyText.decodeUtf8 body
        & XML.parseText_ XML.def
        & HCursor.fromDocument

searchBoardGamesRequest :: NonEmptyText -> Request
searchBoardGamesRequest query =
  HTTP.setQueryString queries request
  where
    qs =
      NonEmptyText.toText query
        & Encoding.encodeUtf8
    request =
      HTTP.defaultRequest
        { host = "api.geekdo.com"
        , port = 443
        , secure = True
        , path = "/xmlapi2/search"
        }
    queries =
      [ ("type", Just "boardgame,boardgameexpansion")
      , ("query", Just qs)
      ]

decodeInManyBoardGames :: DecodeCursor a => HCursor -> DecodeResult [a]
decodeInManyBoardGames cursor =
  DecodeCursor.decodeMany ( cursor %/ HCursor.laxElement "item" )

getBoardGamesRequest :: [BoardGameID] -> Request
getBoardGamesRequest boardGameIds =
  HTTP.setQueryString queries request
  where
    ids =
      show <$> boardGameIds
        & List.intercalate ","
        & Char8.pack
    request =
      HTTP.defaultRequest
        { host = "api.geekdo.com"
        , port = 443
        , secure = True
        , path = "/xmlapi2/thing"
        }
    queries = [("id", Just ids)]
