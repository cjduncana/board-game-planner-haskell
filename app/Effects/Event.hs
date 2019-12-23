module Effects.Event
  ( Event
  , create
  , list
  , runEventAsSQLite
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple (Connection, NamedParam((:=)), Query)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, Maybe, ($), (<>))
import qualified Prelude

import Args.ListEvents (ByLocationArgs)
import Effects.BoardGameGeek (BoardGameGeek)
import Effects.EventGame (EventGame)
import qualified Effects.EventGame as EventGame
import Effects.EventPlayer (EventPlayer)
import qualified Effects.User
import Types.BoardGame (BoardGame, BoardGameID)
import Types.Coordinate (Coordinate)
import qualified Types.Coordinate as Coordinate
import qualified Types.Event as Types
import Types.Time (Time)
import Types.User (User, UserID)
import qualified Types.User as User

data Event m a where
  -- Create a new Event
  Create :: User -> Time -> Coordinate -> [BoardGame] -> Event m Types.Event
  -- List all Events
  List :: Time -> Maybe (NonEmpty BoardGameID) -> Maybe (NonEmpty UserID) -> Maybe ByLocationArgs -> Event m [Types.Event]

Polysemy.makeSem ''Event

runEventAsSQLite ::
  Members [BoardGameGeek, EventGame, EventPlayer, Effects.User.User, Embed IO] r
  => Sem (Event : r) a
  -> Sem (Input Connection : r) a
runEventAsSQLite = Polysemy.reinterpret $ \case

  Create creator startTime location games -> do
    newEvent <- Types.newEvent creator startTime location games
    let id = Types.getID newEvent
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    Polysemy.embed $ SQLite.executeNamed conn query (params id now)
    EventGame.create id games
    Prelude.pure newEvent

    where
      query = Prelude.mconcat
        [ "INSERT INTO " <> eventsTable
        , " "
        , "(id, creatorID, startTime, latitude, longitude, createdAt, updatedAt)"
        , " "
        , "VALUES (:id, :creatorID, :startTime, :latitude, :longitude, :createdAt, :updatedAt)"
        ]

      params id now =
        [ ":id" := id
        , ":creatorID" := User.getID creator
        , ":startTime" := startTime
        , ":latitude" := Coordinate.getLatitude location
        , ":longitude" := Coordinate.getLongitude location
        , ":createdAt" := now
        , ":updatedAt" := now
        ]

  List startAfter _ _ _ -> do
  -- TODO: Implement other parameters
  -- List startAfter byGameIDs byPlayerIDs byLocationArgs ->
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> eventsTable
        , " "
        , "WHERE startTime >= :startAfter"
        ]

      params = [ ":startAfter" := startAfter ]

eventsTable :: Query
eventsTable = "events"
