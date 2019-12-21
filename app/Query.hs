module Query (Query, query) where

import Data.Morpheus.Types (GQLType, Res)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Web.JWT (Signer)

import Resolver.Auth (TokenArgs)
import qualified Resolver.Auth as Auth
import Resolver.BoardGame (BoardGamesArgs)
import qualified Resolver.BoardGame as BoardGame
import Types.BoardGame (BoardGame)

data Query m = Query
  { boardGames :: BoardGamesArgs -> m [BoardGame]
  , token :: TokenArgs -> m Text
  } deriving (Generic, GQLType)

query :: Connection -> NominalDiffTime -> Manager -> Signer -> Query (Res () IO)
query conn daysLater manager signer = Query
  { boardGames = BoardGame.resolveBoardGames manager
  , token = Auth.resolveToken conn daysLater signer
  }
