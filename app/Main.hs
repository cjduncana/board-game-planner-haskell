module Main (main) where

import qualified Control.Monad.IO.Class as Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Morpheus as M
import Data.Morpheus.Types
    (GQLRootResolver(GQLRootResolver), Undefined(Undefined))
import qualified Data.Morpheus.Types as M
import Data.Time.Clock (NominalDiffTime)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.TLS as TLS
import Web.JWT (Signer)
import qualified Web.Scotty as S

import Environment (Environment)
import qualified Environment as Env
import qualified Migration
import Mutation (Mutation)
import qualified Mutation
import Query (Query)
import qualified Query

main :: IO ()
main = do
  manager <- TLS.newTlsManager
  env <- Env.getEnvironment
  SQLite.withConnection (Env.sqliteDatabaseName env) $ \conn -> do
    Migration.run conn
    S.scotty 3000 $
      S.post "/api" $ do
        body <- S.body
        response <- Monad.liftIO $ gqlApi conn env manager $ LazyByteString.toStrict body
        S.raw $ LazyByteString.fromStrict response

gqlApi :: Connection -> Environment -> Manager -> ByteString -> IO ByteString
gqlApi conn env manager =
  M.interpreter $
    rootResolver conn (Env.expireDaysLater env) manager $
      Env.jwtSigner env

rootResolver :: Connection -> NominalDiffTime -> Manager -> Signer -> GQLRootResolver IO () Query Mutation Undefined
rootResolver conn daysLater manager signer =
  GQLRootResolver
    { M.queryResolver = Query.query conn daysLater manager signer
    , M.mutationResolver = Mutation.mutation conn manager signer
    , M.subscriptionResolver = Undefined
    }
