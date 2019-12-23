module Resolver.Event
  ( CreateEventArgs
  , resolveCreateEvent
  , resolveEvents
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Types (MutRes, Res)
import qualified Data.Morpheus.Types as M
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Embed as Embed
import qualified Polysemy.Input as Input
import Polysemy.State (State)
import qualified Polysemy.State as State
import Web.JWT (Signer)

import Args.ListEvents (ListEventsArgs)
import qualified Args.ListEvents as ListEvents
import Effects.BoardGameGeek (BoardGameGeek)
import qualified Effects.BoardGameGeek as BoardGameGeek
import qualified Effects.Event as Effects
import Effects.EventGame (EventGame)
import qualified Effects.EventGame as EventGame
import Effects.EventPlayer (EventPlayer)
import qualified Effects.EventPlayer as EventPlayer
import qualified Effects.User
import Types.BoardGame (BoardGame, BoardGameID)
import Types.Coordinate (Coordinate, Latitude, Longitude)
import qualified Types.Coordinate as Coordinate
import Types.Event (Event)
import Types.JWT (JWT)
import qualified Types.JWT as JWT
import Types.Time (Time)
import qualified Types.Time as Time
import Types.User (User)
import qualified Types.User as User

data CreateEventArgs = CreateEventArgs
  { token :: Text
  , startTime :: Time
  , latitude :: Latitude
  , longitude :: Longitude
  , gameIDs :: [BoardGameID]
  } deriving (Generic)

resolveCreateEvent :: Connection -> Manager -> Signer -> CreateEventArgs -> MutRes () IO Event
resolveCreateEvent conn manager signer CreateEventArgs {token, startTime, latitude, longitude, gameIDs} =
  M.liftEither $
    createEvent signer token startTime (Coordinate.mkCoordinate latitude longitude) gameIDs
      & runEventResolvers conn
      & State.evalState manager
      & Polysemy.runM

-- Add token validation
resolveEvents :: Connection -> Manager -> ListEventsArgs -> Res () IO [Event]
resolveEvents conn manager args =
  Effects.list
    (ListEvents.startAfter args)
    (NonEmpty.nonEmpty =<< ListEvents.byGameIDs args)
    (NonEmpty.nonEmpty =<< ListEvents.byPlayerIDs args)
    (ListEvents.byLocationArgs args)
      & runEventResolvers conn
      & State.evalState manager
      & Embed.runEmbedded M.lift
      & Polysemy.runM

createEvent ::
  Members [Embed IO, BoardGameGeek, Effects.Event, Effects.User.User] r
  => Signer
  -> Text
  -> Time
  -> Coordinate
  -> [BoardGameID]
  -> Sem r (Either String Event)
createEvent signer encodedToken startTime location gameIDs = do
  now <- Time.getNow
  case JWT.decodeAndVerify signer now encodedToken of
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
  case User.getUserIDFromJWT jwt of
    Nothing -> pure Nothing
    Just userID -> Effects.User.findOne userID

findGames :: Member BoardGameGeek r => [BoardGameID] -> Sem r (Maybe [BoardGame])
findGames gameIDs =
  BoardGameGeek.getBoardGames gameIDs
    & fmap (allGamesExists gameIDs)

allGamesExists :: [id] -> [BoardGame] -> Maybe [BoardGame]
allGamesExists gameIDs boardGames =
  if length gameIDs /= length boardGames
    then Nothing
    else Just boardGames

runEventResolvers ::
  Members [State Manager, Embed IO] r
  => Connection
  -> Sem (Effects.Event : BoardGameGeek : Effects.User.User : EventGame : EventPlayer : r) a
  -> Sem r a
runEventResolvers conn =
  Effects.runEventAsSQLite
    >>> Input.runInputConst conn
    >>> BoardGameGeek.runBoardGameGeek
    >>> Effects.User.runUserAsSQLite
    >>> Input.runInputConst conn
    >>> EventGame.runEventGameAsSQLite
    >>> Input.runInputConst conn
    >>> EventPlayer.runEventPlayerAsSQLite
    >>> Input.runInputConst conn
