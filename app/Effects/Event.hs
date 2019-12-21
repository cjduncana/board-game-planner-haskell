module Effects.Event
  ( Event
  , create
  , runEventAsSQLite
  ) where

import Control.Category ((>>>))
import Data.Morpheus.Types (ID(unpackID))
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple (Connection, NamedParam((:=)), Query)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, mconcat, pure, ($), (<>))

import Types.BoardGame (BoardGame)
import Types.Coordinate (Coordinate)
import qualified Types.Coordinate as Coordinate
import qualified Types.Event as Types
import Types.Time (Time)
import Types.User (User)
import qualified Types.User as User
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
    let query = mconcat [ "INSERT INTO " <> eventsTable
                        , " "
                        , "(id, creatorId, startTime, latitude, longitude, createdAt, updatedAt)"
                        , " "
                        , "VALUES (:id, :creatorId, :startTime, :latitude, :longitude, :createdAt, :updatedAt)"
                        ]
    let params = [ ":id" := id
                 , ":creatorId" := (User.getID >>> unpackID) creator
                 , ":startTime" := startTime
                 , ":latitude" := Coordinate.getLatitude location
                 , ":longitude" := Coordinate.getLongitude location
                 , ":createdAt" := now
                 , ":updatedAt" := now
                 ]
    Polysemy.embed $ SQLite.executeNamed conn query params
    -- TODO: Tie players and games
    pure $ Types.create id creator startTime location games

eventsTable :: Query
eventsTable = "events"
