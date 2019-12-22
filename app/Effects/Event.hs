module Effects.Event
  ( Event
  , create
  , runEventAsSQLite
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple (Connection, NamedParam((:=)), Query)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, fmap, mconcat, pure, ($), (<>))

import Types.BoardGame (BoardGame)
import qualified Types.BoardGame as BoardGame
import Types.Coordinate (Coordinate)
import qualified Types.Coordinate as Coordinate
import qualified Types.Event as Types
import Types.Time (Time)
import Types.User (User)
import qualified Types.User as User
import Types.UUID (UUID)
import qualified Types.UUID as UUID

data Event m a where
  -- Create a new Event
  Create :: User -> Time -> Coordinate -> [BoardGame] -> Event m Types.Event

Polysemy.makeSem ''Event

runEventAsSQLite :: Member (Embed IO) r => Sem (Event : r) a -> Sem (Input Connection : r) a
runEventAsSQLite = Polysemy.reinterpret $ \case

  Create creator startTime location games -> do
    id <- UUID.randomUUID
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    Polysemy.embed $ SQLite.executeNamed conn eventsQuery (eventsParams id now)
    Polysemy.embed $ SQLite.executeNamed conn eventsPlayersQuery (eventsPlayersParams id)
    fmap (eventsGamesParams id >>> SQLite.executeNamed conn eventsGamesQuery) games
      & mconcat
      & Polysemy.embed
    pure $ Types.create id creator startTime location games

    where
      eventsQuery = mconcat
        [ "INSERT INTO " <> eventsTable
        , " "
        , "(id, creatorId, startTime, latitude, longitude, createdAt, updatedAt)"
        , " "
        , "VALUES (:id, :creatorId, :startTime, :latitude, :longitude, :createdAt, :updatedAt)"
        ]

      eventsParams id now =
        [ ":id" := id
        , ":creatorId" := User.getID creator
        , ":startTime" := startTime
        , ":latitude" := Coordinate.getLatitude location
        , ":longitude" := Coordinate.getLongitude location
        , ":createdAt" := now
        , ":updatedAt" := now
        ]

      eventsPlayersQuery = mconcat
        [ "INSERT INTO " <> eventsPlayersTable
        , " "
        , "(eventId, playerId)"
        , " "
        , "VALUES (:eventId, :playerId)"
        ]

      eventsPlayersParams :: UUID -> [NamedParam]
      eventsPlayersParams eventId =
        [ ":eventId" := eventId
        , ":playerId" := User.getID creator
        ]

      eventsGamesQuery = mconcat
        [ "INSERT INTO " <> eventsGamesTable
        , " "
        , "(eventId, gameId)"
        , " "
        , "VALUES (:eventId, :gameId)"
        ]

      eventsGamesParams :: UUID -> BoardGame -> [NamedParam]
      eventsGamesParams eventId boardGame =
        [ ":eventId" := eventId
        , ":gameId" := BoardGame.getID boardGame
        ]

eventsTable :: Query
eventsTable = "events"

eventsPlayersTable :: Query
eventsPlayersTable = "eventsPlayers"

eventsGamesTable :: Query
eventsGamesTable = "eventsGames"
