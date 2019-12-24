module Effects.EventGame
  ( EventGame
  , create
  , list
  , runEventGameAsSQLite
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.SQLite.Simple (Connection, NamedParam((:=)))
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, Maybe(Just, Nothing), ($), (<$>), (<>))
import qualified Prelude

import qualified Migration
import Types.BoardGame (BoardGame, BoardGameID)
import qualified Types.BoardGame as BoardGame
import Types.EventID (EventID)
import qualified Types.UUID as UUID

data EventGame m a where
  -- Tie several Board Games to an Event
  Create :: EventID -> [BoardGame] -> EventGame m ()
  -- List all Board Game IDs per Event
  List :: [EventID] -> EventGame m (Map EventID [BoardGameID])

Polysemy.makeSem ''EventGame

runEventGameAsSQLite :: Member (Embed IO) r => Sem (EventGame : r) a -> Sem (Input Connection : r) a
runEventGameAsSQLite = Polysemy.reinterpret $ \case

  Create eventID games -> do
    conn <- Input.input
    Prelude.fmap (params >>> SQLite.executeNamed conn query) games
      & Prelude.mconcat
      & Polysemy.embed

    where

      query = Prelude.mconcat
        [ "INSERT INTO " <> Migration.eventsGamesTable
        , " "
        , "(eventID, gameID)"
        , " "
        , "VALUES (:eventID, :gameID)"
        ]

      params :: BoardGame -> [NamedParam]
      params boardGame =
        [ ":eventID" := eventID
        , ":gameID" := BoardGame.getID boardGame
        ]

  List eventIDs -> do
    conn <- Input.input
    Polysemy.embed (groupGameIDsByEventID <$> SQLite.query_ conn query)

    where
      query = Prelude.mconcat
        [ "SELECT eventID, gameID"
        , " "
        , "FROM " <> Migration.eventsGamesTable
        , " "
        , "WHERE eventID IN (" <> UUID.idsToQuery eventIDs <> ")"
        ]

groupGameIDsByEventID :: [(EventID, BoardGameID)] -> Map EventID [BoardGameID]
groupGameIDsByEventID = List.foldl' addGameID Map.empty

addGameID :: Map EventID [BoardGameID] -> (EventID, BoardGameID) -> Map EventID [BoardGameID]
addGameID groupGameIDs (eventID, gameID) =
  Map.alter alterMap eventID groupGameIDs
  where
    alterMap :: Maybe [BoardGameID] -> Maybe [BoardGameID]
    alterMap Nothing = Prelude.pure $ Prelude.pure gameID
    alterMap (Just gameIDs) = Prelude.pure $ gameID : gameIDs
