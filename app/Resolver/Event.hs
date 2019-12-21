module Resolver.Event (CreateEventArgs, resolveCreateEvent) where

import Data.Function ((&))
import Data.Morpheus.Types (ID(unpackID), MutRes)
import qualified Data.Morpheus.Types as M
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Polysemy (Member, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Input as Input
import qualified Polysemy.State as State
import Web.JWT (Signer)
import qualified Web.JWT as JWT

import Effects.BoardGameGeek (BoardGameGeek)
import qualified Effects.BoardGameGeek as BoardGameGeek
import qualified Effects.Event as Effects
import qualified Effects.User
import Types.BoardGame (BoardGame)
import Types.Coordinate (Coordinate, Latitude, Longitude)
import qualified Types.Coordinate as Coordinate
import Types.Event (Event)
import Types.JWT (JWT)
import qualified Types.JWT as JWT
import Types.Time (Time)
import Types.User (User)
import qualified Types.User as User
import qualified Types.UUID as UUID

data CreateEventArgs = CreateEventArgs
  { token :: Text
  , startTime :: Time
  , latitude :: Latitude
  , longitude :: Longitude
  , gameIDs :: [ID]
  } deriving (Generic)

resolveCreateEvent :: Connection -> Manager -> Signer -> CreateEventArgs -> MutRes () IO Event
resolveCreateEvent conn manager signer CreateEventArgs {token, startTime, latitude, longitude, gameIDs} =
  M.liftEither $
    createEvent signer token startTime (Coordinate.mkCoordinate latitude longitude) gameIDs
      & BoardGameGeek.runBoardGameGeek
      & State.evalState manager
      & Effects.User.runUserAsSQLite
      & Input.runInputConst conn
      & Effects.runEventAsSQLite
      & Input.runInputConst conn
      & Polysemy.runM

createEvent ::
  Members [BoardGameGeek, Effects.Event, Effects.User.User] r
  => Signer
  -> Text
  -> Time
  -> Coordinate
  -> [ID]
  -> Sem r (Either String Event)
createEvent signer encodedToken startTime location gameIDs =
  case JWT.decodeAndVerify signer encodedToken of
    Left jwtError -> pure $ Left $ show jwtError
    Right jwt -> do
      maybeUser <- findUser jwt
      case maybeUser of
        Nothing -> pure $ Left "User not found"
        Just user -> do
          maybeBoardGames <- findGames gameIDs
          case maybeBoardGames of
            Nothing -> pure $ Left "Board Games not found"
            Just boardGames ->
              Right <$> Effects.create user startTime location boardGames

findUser :: Member Effects.User.User r => JWT -> Sem r (Maybe User)
findUser jwt =
  case maybeUUID of
    Nothing -> pure Nothing
    Just userID -> do
      maybeUser <- Effects.User.find userID
      pure (fst . User.intoTuple <$> maybeUser)
  where
    maybeUUID =
      JWT.claims jwt
        & JWT.sub
        & fmap JWT.stringOrURIToText
        & (=<<) UUID.fromText

findGames :: Member BoardGameGeek r => [ID] -> Sem r (Maybe [BoardGame])
findGames gameIDs =
  case sequence (BoardGameGeek.mkBoardGameID . unpackID <$> gameIDs) of
    Nothing -> pure Nothing
    Just boardGameIDs ->
      BoardGameGeek.getBoardGames boardGameIDs
        & fmap (allGamesExists gameIDs)

allGamesExists :: [ID] -> [BoardGame] -> Maybe [BoardGame]
allGamesExists gameIDs boardGames =
  if length gameIDs /= length boardGames
    then Nothing
    else Just boardGames
