module Resolver.Event
  ( CreateEventArgs
  , resolveCreateEvent
  , resolveEvents
  ) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Types (MutRes, Res)
import qualified Data.Morpheus.Types as M
import Data.Text (Text)
import Database.MySQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Polysemy (Embed, Member, Members, Sem)
import qualified Polysemy
import qualified Polysemy.Input as Input
import Polysemy.State (State)
import qualified Polysemy.State as State
import Web.JWT (Signer)

import Args.ListEvents (ByLocationArgs, ListEventsArgs)
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
import qualified Types.JWT as JWT
import Types.Time (Time)
import Types.User (UserID)

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

resolveEvents :: Connection -> Manager -> Signer -> ListEventsArgs -> Res () IO [Event]
resolveEvents conn manager signer args =
  M.liftEither $
    listEvents
      signer
      (ListEvents.token args)
      (ListEvents.startAfter args)
      (NonEmpty.nonEmpty =<< ListEvents.byGameIDs args)
      (NonEmpty.nonEmpty =<< ListEvents.byPlayerIDs args)
      (ListEvents.byLocationArgs args)
        & runEventResolvers conn
        & State.evalState manager
        & Polysemy.runM

createEvent ::
  Members [Embed IO, BoardGameGeek, Effects.Event, Effects.User.User] r
  => Signer
  -> Text
  -> Time
  -> Coordinate
  -> [BoardGameID]
  -> Sem r (Either String Event)
createEvent signer encodedToken startTime location gameIDs =
  JWT.use signer encodedToken $ \user -> do
    maybeBoardGames <- findGames gameIDs
    case maybeBoardGames of
      Nothing -> pure $ Left "Board Games not found"
      Just boardGames ->
        Right <$> Effects.create user startTime location boardGames

listEvents ::
  Members [Embed IO, Effects.Event, Effects.User.User] r
  => Signer
  -> Text
  -> Time
  -> Maybe (NonEmpty BoardGameID)
  -> Maybe (NonEmpty UserID)
  -> Maybe ByLocationArgs
  -> Sem r (Either String [Event])
listEvents signer encodedToken startAfter byGameIDs byPlayerIDs byLocationArgs =
  JWT.use signer encodedToken $ \_ ->
    Right <$> Effects.list startAfter byGameIDs byPlayerIDs byLocationArgs

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
  Effects.runEventAsMySQL
    >>> Input.runInputConst conn
    >>> BoardGameGeek.runBoardGameGeek
    >>> Effects.User.runUserAsMySQL
    >>> Input.runInputConst conn
    >>> EventGame.runEventGameAsMySQL
    >>> Input.runInputConst conn
    >>> EventPlayer.runEventPlayerAsMySQL
    >>> Input.runInputConst conn
