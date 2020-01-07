module Migration
  ( eventsTable
  , eventsGamesTable
  , eventsPlayersTable
  , run
  , usersTable
  ) where

import qualified Control.Monad as Monad
import Data.Function ((&))
import Database.MySQL.Simple (Connection, Query)
import qualified Database.MySQL.Simple as MySQL

run :: Connection -> IO ()
run conn =
  allMigrations
    & fmap (conn &)
    & mconcat

allMigrations :: [Connection -> IO ()]
allMigrations =
  [ createUsersTable
  , createEventsTable
  ]

createUsersTable :: Connection -> IO ()
createUsersTable conn =
  MySQL.execute_ conn query
    & Monad.void
  where
    query = mconcat
      [ "CREATE TABLE IF NOT EXISTS " <> usersTable
      , " "
      , "( id CHAR(36) PRIMARY KEY NOT NULL"
      , ", name TEXT NOT NULL"
      , ", email VARCHAR(255) NOT NULL UNIQUE"
      , ", hashedPassword TEXT NOT NULL"
      , ", createdAt DATETIME NOT NULL"
      , ", updatedAt DATETIME NOT NULL"
      , ")"
      ]

createEventsTable :: Connection -> IO ()
createEventsTable conn =
  Monad.mapM_
    (MySQL.execute_ conn)
    [eventsQuery, eventsPlayersQuery, eventsGamesQuery]
  where
    eventsQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS " <> eventsTable
      , " "
      , "( id CHAR(36) PRIMARY KEY NOT NULL"
      , ", creatorID TEXT NOT NULL"
      , ", startTime DATETIME NOT NULL"
      , ", latitude FLOAT NOT NULL"
      , ", longitude FLOAT NOT NULL"
      , ", createdAt DATETIME NOT NULL"
      , ", updatedAt DATETIME NOT NULL"
      , ")"
      ]

    eventsPlayersQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS " <> eventsPlayersTable
      , " "
      , "( eventID CHAR(36) NOT NULL"
      , ", playerID CHAR(36) NOT NULL"
      , ")"
      ]

    eventsGamesQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS " <> eventsGamesTable
      , " "
      , "( eventID CHAR(36) NOT NULL"
      , ", gameID MEDIUMINT UNSIGNED NOT NULL"
      , ")"
      ]

eventsTable :: Query
eventsTable = "events"

eventsGamesTable :: Query
eventsGamesTable = "events_games"

eventsPlayersTable :: Query
eventsPlayersTable = "events_players"

usersTable :: Query
usersTable = "users"
