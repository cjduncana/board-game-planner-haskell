module Effects.EventGame
  ( EventGame
  , create
  , list
  , runEventGameAsMySQL
  ) where

import qualified Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.MySQL.Simple (Connection, In(In), Only(Only))
import qualified Database.MySQL.Simple as MySQL
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

data EventGame m a where
  -- Tie several Board Games to an Event
  Create :: EventID -> [BoardGame] -> EventGame m ()
  -- List all Board Game IDs per Event
  List :: [EventID] -> EventGame m (Map EventID [BoardGameID])

Polysemy.makeSem ''EventGame

runEventGameAsMySQL :: Member (Embed IO) r => Sem (EventGame : r) a -> Sem (Input Connection : r) a
runEventGameAsMySQL = Polysemy.reinterpret $ \case

  Create eventID games -> do
    conn <- Input.input
    Prelude.fmap params games
      & MySQL.executeMany conn query
      & Monad.void
      & Polysemy.embed

    where

      query = Prelude.mconcat
        [ "INSERT INTO " <> Migration.eventsGamesTable
        , " "
        , "(eventID, gameID)"
        , " "
        , "VALUES (?, ?)"
        ]

      params :: BoardGame -> (EventID, BoardGameID)
      params boardGame = (eventID, BoardGame.getID boardGame)

  List eventIDs -> do
    conn <- Input.input
    Polysemy.embed (groupGameIDsByEventID <$> MySQL.query conn query params)

    where
      query = Prelude.mconcat
        [ "SELECT eventID, gameID"
        , " "
        , "FROM " <> Migration.eventsGamesTable
        , " "
        , "WHERE eventID IN ?"
        ]
      params = Only (In eventIDs)

groupGameIDsByEventID :: [(EventID, BoardGameID)] -> Map EventID [BoardGameID]
groupGameIDsByEventID = List.foldl' addGameID Map.empty

addGameID :: Map EventID [BoardGameID] -> (EventID, BoardGameID) -> Map EventID [BoardGameID]
addGameID groupGameIDs (eventID, gameID) =
  Map.alter alterMap eventID groupGameIDs
  where
    alterMap :: Maybe [BoardGameID] -> Maybe [BoardGameID]
    alterMap Nothing = Prelude.pure $ Prelude.pure gameID
    alterMap (Just gameIDs) = Prelude.pure $ gameID : gameIDs
