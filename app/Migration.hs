module Migration (run) where

import Data.Function ((&))
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite

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
  SQLite.execute_ conn query
  where
    query = mconcat
      [ "CREATE TABLE IF NOT EXISTS users"
      , " "
      , "( id TEXT PRIMARY KEY NOT NULL"
      , ", name TEXT NOT NULL"
      , ", email TEXT NOT NULL UNIQUE"
      , ", hashedPassword TEXT NOT NULL"
      , ", createdAt TEXT NOT NULL"
      , ", updatedAt TEXT NOT NULL"
      , ") WITHOUT ROWID"
      ]

createEventsTable :: Connection -> IO ()
createEventsTable conn =
  [eventsQuery, eventsPlayersQuery, eventsGamesQuery]
    & fmap (SQLite.execute_ conn)
    & mconcat
  where
    eventsQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS events"
      , " "
      , "( id TEXT PRIMARY KEY NOT NULL"
      , ", creatorId TEXT NOT NULL"
      , ", startTime TEXT NOT NULL"
      , ", latitude FLOAT NOT NULL"
      , ", longitude FLOAT NOT NULL"
      , ", createdAt TEXT NOT NULL"
      , ", updatedAt TEXT NOT NULL"
      , ") WITHOUT ROWID"
      ]

    eventsPlayersQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS eventsPlayers"
      , " "
      , "( eventId TEXT NOT NULL"
      , ", playerId TEXT NOT NULL"
      , ")"
      ]

    eventsGamesQuery = mconcat
      [ "CREATE TABLE IF NOT EXISTS eventsGames"
      , " "
      , "( eventId TEXT NOT NULL"
      , ", gameId TEXT NOT NULL"
      , ")"
      ]
