module Effects.Event
  ( Event
  , create
  , list
  , runEventAsMySQL
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Time.Clock as Time
import Database.MySQL.Simple (Connection, In(In), Only(Only))
import qualified Database.MySQL.Simple as MySQL
import Polysemy (Embed, Members, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, Maybe(Just, Nothing), ($), (<>))
import qualified Prelude

import Args.ListEvents (ByLocation)
import qualified Args.ListEvents as ListEvents
import Effects.BoardGameGeek (BoardGameGeek)
import Effects.EventGame (EventGame)
import qualified Effects.EventGame as EventGame
import Effects.EventPlayer (EventPlayer)
import qualified Effects.User
import qualified Migration
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
  List :: Time -> Maybe (NonEmpty BoardGameID) -> Maybe (NonEmpty UserID) -> Maybe ByLocation -> Event m [Types.Event]

Polysemy.makeSem ''Event

runEventAsMySQL ::
  Members [BoardGameGeek, EventGame, EventPlayer, Effects.User.User, Embed IO] r
  => Sem (Event : r) a
  -> Sem (Input Connection : r) a
runEventAsMySQL = Polysemy.reinterpret $ \case

  Create creator startTime location games -> do
    newEvent <- Types.newEvent creator startTime location games
    let id = Types.getID newEvent
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    _ <- Polysemy.embed $ MySQL.execute conn query (params id now)
    EventGame.create id games
    Prelude.pure newEvent

    where
      query = Prelude.mconcat
        [ "INSERT INTO " <> Migration.eventsTable
        , " "
        , "(id, creatorID, startTime, latitude, longitude, createdAt, updatedAt)"
        , " "
        , "VALUES (?, ?, ?, ?, ?, ?, ?)"
        ]

      params id now =
        ( id
        , User.getID creator
        , startTime
        , Coordinate.getLatitude location
        , Coordinate.getLongitude location
        , now
        , now
        )

  List startAfter Nothing Nothing Nothing -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        ]

      params = Only startAfter

  List startAfter Nothing Nothing (Just byLocation) -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , "acos(sin(?) * sin(latitude)"
        , " + "
        , "cos(?) * cos(latitude) * cos(longitude - (?))) * 6371"
        , " <= "
        , "?"
        ]

      latitude = ListEvents.latitude byLocation

      params =
        ( startAfter
        , latitude
        , latitude
        , ListEvents.longitude byLocation
        , ListEvents.distanceInKm byLocation
        )

  List startAfter (Just gameIDs) Nothing Nothing -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsGamesTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsGamesTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsGamesTable <> ".gameID IN ?"
        ]

      params = (startAfter, In (NonEmpty.toList gameIDs))

  List startAfter (Just gameIDs) Nothing (Just byLocation) -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsGamesTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsGamesTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsGamesTable <> ".gameID IN ?"
        , " AND "
        , "acos(sin(?) * sin(latitude)"
        , " + "
        , "cos(?) * cos(latitude) * cos(longitude - (?))) * 6371"
        , " <= "
        , "?"
        ]

      latitude = ListEvents.latitude byLocation

      params =
        ( startAfter
        , In (NonEmpty.toList gameIDs)
        , latitude
        , latitude
        , ListEvents.longitude byLocation
        , ListEvents.distanceInKm byLocation
        )

  List startAfter Nothing (Just playerIDs) Nothing -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsPlayersTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsPlayersTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsPlayersTable <> ".playerID IN ?"
        ]

      params = (startAfter, In (NonEmpty.toList playerIDs))

  List startAfter Nothing (Just playerIDs) (Just byLocation) -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsPlayersTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsPlayersTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsPlayersTable <> ".playerID IN ?"
        , " AND "
        , "acos(sin(?) * sin(latitude)"
        , " + "
        , "cos(?) * cos(latitude) * cos(longitude - (?))) * 6371"
        , " <= "
        , "?"
        ]

      latitude = ListEvents.latitude byLocation

      params =
        ( startAfter
        , In (NonEmpty.toList playerIDs)
        , latitude
        , latitude
        , ListEvents.longitude byLocation
        , ListEvents.distanceInKm byLocation
        )

  List startAfter (Just gameIDs) (Just playerIDs) Nothing -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsPlayersTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsPlayersTable <> ".eventID"
        , " "
        , "INNER JOIN " <> Migration.eventsGamesTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsGamesTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsGamesTable <> ".gameID IN ?"
        , " AND "
        , Migration.eventsPlayersTable <> ".playerID IN ?"
        ]

      params = (startAfter, In (NonEmpty.toList gameIDs), In (NonEmpty.toList playerIDs))

  List startAfter (Just gameIDs) (Just playerIDs) (Just byLocation) -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, creatorID, startTime, latitude, longitude"
        , " "
        , "FROM " <> Migration.eventsTable
        , " "
        , "INNER JOIN " <> Migration.eventsPlayersTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsPlayersTable <> ".eventID"
        , " "
        , "INNER JOIN " <> Migration.eventsGamesTable
        , " ON "
        , Migration.eventsTable <> ".id"
        , " = "
        , Migration.eventsGamesTable <> ".eventID"
        , " "
        , "WHERE"
        , " "
        , "startTime >= ?"
        , " AND "
        , Migration.eventsGamesTable <> ".gameID IN ?"
        , " AND "
        , Migration.eventsPlayersTable <> ".playerID IN ?"
        , " AND "
        , "acos(sin(?) * sin(latitude)"
        , " + "
        , "cos(?) * cos(latitude) * cos(longitude - (?))) * 6371"
        , " <= "
        , "?"
        ]

      latitude = ListEvents.latitude byLocation

      params =
        ( startAfter
        , In (NonEmpty.toList gameIDs)
        , In (NonEmpty.toList playerIDs)
        , latitude
        , latitude
        , ListEvents.longitude byLocation
        , ListEvents.distanceInKm byLocation
        )
