module Types.Event
  ( Event
  , findMany
  , getID
  , newEvent
  ) where

import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Morpheus.Types (GQLType)
import Data.Set (Set)
import qualified Data.Set as Set
import Database.MySQL.Simple (Connection, Query)
import qualified Database.MySQL.Simple as MySQL
import Database.MySQL.Simple.QueryParams (QueryParams)
import GHC.Generics (Generic)
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy
import Prelude (IO, Maybe, Ord, ($), (<$>), (<*>))
import qualified Prelude

import Effects.BoardGameGeek (BoardGameGeek)
import qualified Effects.BoardGameGeek as BoardGameGeek
import Effects.EventGame (EventGame)
import qualified Effects.EventGame as EventGame
import Effects.EventPlayer (EventPlayer)
import qualified Effects.EventPlayer as EventPlayer
import qualified Effects.User as User
import Types.BoardGame (BoardGame, BoardGameID)
import qualified Types.BoardGame as BoardGame
import Types.Coordinate (Coordinate, Latitude, Longitude)
import qualified Types.Coordinate as Coordinate
import Types.EventID (EventID)
import qualified Types.EventID as EventID
import Types.Time (Time)
import Types.User (User, UserID)
import qualified Types.User

data Event = Event
  { id :: EventID
  , creator :: User
  , startTime :: Time
  , location :: Coordinate
  , players :: [User]
  , games :: [BoardGame]
  } deriving (Generic, GQLType)

getID :: Event -> EventID
getID = id

newEvent ::
  Member (Embed IO) r
  => User
  -> Time
  -> Coordinate
  -> [BoardGame]
  -> Sem r Event
newEvent creator startTime location games =
  Event
    <$> EventID.new
    <*> Prelude.pure creator
    <*> Prelude.pure startTime
    <*> Prelude.pure location
    <*> Prelude.pure []
    <*> Prelude.pure games

findMany ::
  (Members [BoardGameGeek, EventGame, EventPlayer, User.User, Embed IO] r, QueryParams q)
  => Connection
  -> Query
  -> q
  -> Sem r [Event]
findMany conn query params = do
  results <- Polysemy.embed $ MySQL.query conn query params
  let (eventIDs, creatorIDs) = gatherIDs results
  playerIDsInEvents <- EventPlayer.list eventIDs
  let userIDs = gatherSomeIDs creatorIDs playerIDsInEvents
  users <- User.list $ Set.toList userIDs
  gameIDsInEvents <- EventGame.list eventIDs
  let gameIDs = gatherSomeIDs Set.empty gameIDsInEvents
  games <- BoardGameGeek.getBoardGames $ Set.toList gameIDs
  Prelude.pure $
    buildEvents results playerIDsInEvents gameIDsInEvents users games

buildEvents ::
  [(EventID, UserID, Time, Latitude, Longitude)]
  -> Map EventID [UserID]
  -> Map EventID [BoardGameID]
  -> [User]
  -> [BoardGame]
  -> [Event]
buildEvents results playerIDsInEvents gameIDsInEvents users games =
  Maybe.mapMaybe
    (buildEvent playerIDsInEvents gameIDsInEvents usersPerID gamesPerID)
    results

  where
    usersPerID :: Map UserID User
    usersPerID =
      List.foldl' (addSomething Types.User.getID) Map.empty users

    gamesPerID :: Map BoardGameID BoardGame
    gamesPerID =
      List.foldl' (addSomething BoardGame.getID) Map.empty games

buildEvent ::
  Map EventID [UserID]
  -> Map EventID [BoardGameID]
  -> Map UserID User
  -> Map BoardGameID BoardGame
  -> (EventID, UserID, Time, Latitude, Longitude)
  -> Maybe Event
buildEvent
  playerIDsInEvents
  gameIDsInEvents
  usersPerID
  gamesPerID
  (id, creatorID, startTime, latitude, longitude)
  =
    Event
      <$> Prelude.pure id
      <*> Map.lookup creatorID usersPerID
      <*> Prelude.pure startTime
      <*> Prelude.pure (Coordinate.mkCoordinate latitude longitude)
      <*> maybePlayers
      <*> maybeGames

    where
      maybePlayers :: Maybe [User]
      maybePlayers =
        Map.findWithDefault [] id playerIDsInEvents
          & Prelude.mapM (`Map.lookup` usersPerID)

      maybeGames :: Maybe [BoardGame]
      maybeGames =
        Map.findWithDefault [] id gameIDsInEvents
          & Prelude.mapM (`Map.lookup` gamesPerID)

gatherIDs :: [(EventID, UserID, Time, Latitude, Longitude)] -> ([EventID], Set UserID)
gatherIDs = List.foldl' addIDs ([], Set.empty)

addIDs :: ([EventID], Set UserID) -> (EventID, UserID, Time, Latitude, Longitude) -> ([EventID], Set UserID)
addIDs (gatheredEventIDs, gatheredCreatorIDs) (id, creatorID, _, _, _) =
  (id : gatheredEventIDs, Set.insert creatorID gatheredCreatorIDs)

gatherSomeIDs :: Ord a => Set a -> Map EventID [a] -> Set a
gatherSomeIDs = Map.foldl' addSomeIDs

addSomeIDs :: Ord a => Set a -> [a] -> Set a
addSomeIDs idsInSet idsInList =
  Set.fromList idsInList
    & Set.union idsInSet

addSomething :: Ord id => (something -> id) -> Map id something -> something -> Map id something
addSomething fn map something =
  Map.insert (fn something) something map
