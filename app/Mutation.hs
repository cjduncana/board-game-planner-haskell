module Mutation (Mutation, mutation) where

import Data.Morpheus.Types (GQLType, MutRes)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Web.JWT (Signer)

import Resolver.Event (CreateEventArgs)
import qualified Resolver.Event as Event
import Resolver.User (CreateUserArgs)
import qualified Resolver.User as User
import Types.Event (Event)
import Types.User (User)

data Mutation m = Mutation
  { createEvent :: CreateEventArgs -> m Event
  , createUser :: CreateUserArgs -> m User
  } deriving (Generic, GQLType)

mutation :: Connection -> Manager -> Signer -> Mutation (MutRes () IO)
mutation conn manager signer = Mutation
  { createEvent = Event.resolveCreateEvent conn manager signer
  , createUser = User.resolveCreateUser conn
  }
