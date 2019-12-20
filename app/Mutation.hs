module Mutation (Mutation, mutation) where

import Data.Morpheus.Types (GQLType, MutRes)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)

import Resolver.User (CreateUserArgs)
import qualified Resolver.User as User
import Types.User (User)

newtype Mutation m = Mutation
  { createUser :: CreateUserArgs -> m User
  } deriving (Generic, GQLType)

mutation :: Connection -> Mutation (MutRes () IO)
mutation conn = Mutation
  { createUser = User.resolveCreateUser conn
  }
