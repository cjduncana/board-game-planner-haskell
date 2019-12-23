module Effects.EventPlayer
  ( EventPlayer
  , create
  , list
  , runEventPlayerAsSQLite
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.SQLite.Simple (Connection, NamedParam((:=)), Query)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, Maybe(Just, Nothing), String, ($), (<$>), (<>))
import qualified Prelude

import Types.EventID (EventID)
import Types.User (User, UserID)
import qualified Types.User as User

data EventPlayer m a where
  -- Tie several Players to an Event
  Create :: EventID -> [User] -> EventPlayer m ()
  -- List all User IDs per Event
  List :: [EventID] -> EventPlayer m (Map EventID [UserID])

Polysemy.makeSem ''EventPlayer

runEventPlayerAsSQLite :: Member (Embed IO) r => Sem (EventPlayer : r) a -> Sem (Input Connection : r) a
runEventPlayerAsSQLite = Polysemy.reinterpret $ \case

  Create eventID players -> do
    conn <- Input.input
    Prelude.fmap (params >>> SQLite.executeNamed conn query) players
      & Prelude.mconcat
      & Polysemy.embed

    where

      query = Prelude.mconcat
        [ "INSERT INTO " <> eventsPlayersTable
        , " "
        , "(eventID, gameID)"
        , " "
        , "VALUES (:eventID, :gameID)"
        ]

      params :: User -> [NamedParam]
      params player =
        [ ":eventID" := eventID
        , ":gameID" := User.getID player
        ]

  List eventIDs -> do
    conn <- Input.input
    Polysemy.embed (groupPlayerIDsByEventID <$> SQLite.queryNamed conn query params)

    where
      query = Prelude.mconcat
        [ "SELECT eventID, playerID"
        , " "
        , "FROM " <> eventsPlayersTable
        , " "
        , "WHERE eventID IN (:eventIDs)"
        ]
      params = [ ":eventIDs" := eventIDsParam ]

      eventIDsParam :: String
      eventIDsParam =
        Prelude.show <$> eventIDs
          & List.intercalate ", "

eventsPlayersTable :: Query
eventsPlayersTable = "eventsPlayers"

groupPlayerIDsByEventID :: [(EventID, UserID)] -> Map EventID [UserID]
groupPlayerIDsByEventID = List.foldl' addplayerID Map.empty

addplayerID :: Map EventID [UserID] -> (EventID, UserID) -> Map EventID [UserID]
addplayerID groupplayerIDs (eventID, playerID) =
  Map.alter alterMap eventID groupplayerIDs
  where
    alterMap :: Maybe [UserID] -> Maybe [UserID]
    alterMap Nothing = Prelude.pure $ Prelude.pure playerID
    alterMap (Just playerIDs) = Prelude.pure $ playerID : playerIDs

