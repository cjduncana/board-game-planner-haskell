module Effects.EventPlayer
  ( EventPlayer
  , create
  , list
  , runEventPlayerAsMySQL
  ) where

import qualified Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.MySQL.Simple (Connection, In(In), Only(Only))
import qualified Database.MySQL.Simple as MySQL
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (IO, Maybe(Just, Nothing), ($), (<$>), (<>))
import qualified Prelude

import qualified Migration
import Types.EventID (EventID)
import Types.User (User, UserID)
import qualified Types.User as User

data EventPlayer m a where
  -- Tie several Players to an Event
  Create :: EventID -> [User] -> EventPlayer m ()
  -- List all User IDs per Event
  List :: [EventID] -> EventPlayer m (Map EventID [UserID])

Polysemy.makeSem ''EventPlayer

runEventPlayerAsMySQL :: Member (Embed IO) r => Sem (EventPlayer : r) a -> Sem (Input Connection : r) a
runEventPlayerAsMySQL = Polysemy.reinterpret $ \case

  Create eventID players -> do
    conn <- Input.input
    Prelude.fmap params players
      & MySQL.executeMany conn query
      & Monad.void
      & Polysemy.embed

    where

      query = Prelude.mconcat
        [ "INSERT INTO " <> Migration.eventsPlayersTable
        , " "
        , "(eventID, playerID)"
        , " "
        , "VALUES (?, ?)"
        ]

      params :: User -> (EventID, UserID)
      params player = (eventID, User.getID player)

  List eventIDs -> do
    conn <- Input.input
    Polysemy.embed (groupPlayerIDsByEventID <$> MySQL.query conn query params)

    where
      query = Prelude.mconcat
        [ "SELECT eventID, playerID"
        , " "
        , "FROM " <> Migration.eventsPlayersTable
        , " "
        , "WHERE eventID IN ?"
        ]
      params = Only (In eventIDs)

groupPlayerIDsByEventID :: [(EventID, UserID)] -> Map EventID [UserID]
groupPlayerIDsByEventID = List.foldl' addplayerID Map.empty

addplayerID :: Map EventID [UserID] -> (EventID, UserID) -> Map EventID [UserID]
addplayerID groupplayerIDs (eventID, playerID) =
  Map.alter alterMap eventID groupplayerIDs
  where
    alterMap :: Maybe [UserID] -> Maybe [UserID]
    alterMap Nothing = Prelude.pure $ Prelude.pure playerID
    alterMap (Just playerIDs) = Prelude.pure $ playerID : playerIDs

