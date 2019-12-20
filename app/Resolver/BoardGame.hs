module Resolver.BoardGame
  ( BoardGamesArgs
  , resolveBoardGames
  ) where

import Data.Function ((&))
import Data.Morpheus.Types (Res)
import qualified Data.Morpheus.Types as M
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Polysemy (Member, Sem)
import qualified Polysemy
import qualified Polysemy.Embed as Embed
import qualified Polysemy.State as State

import Effects.BoardGameGeek (BoardGameGeek)
import qualified Effects.BoardGameGeek as BoardGameGeek
import Types.BoardGame (BoardGame)
import Types.NonEmptyText (NonEmptyText)

newtype BoardGamesArgs = BoardGamesArgs
  { query :: NonEmptyText
  } deriving (Generic)

resolveBoardGames :: Manager -> BoardGamesArgs -> Res () IO [BoardGame]
resolveBoardGames manager BoardGamesArgs {query} =
  boardGames query
    & BoardGameGeek.runBoardGameGeek
    & State.evalState manager
    & Embed.runEmbedded M.lift
    & Polysemy.runM

boardGames :: Member BoardGameGeek r => NonEmptyText -> Sem r [BoardGame]
boardGames query =
  BoardGameGeek.searchBoardGameIDsByQuery query >>= BoardGameGeek.getBoardGames
