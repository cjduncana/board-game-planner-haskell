module Main (main) where

import qualified Control.Monad.IO.Class as Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Morpheus as M
import Data.Morpheus.Types
    (GQLRootResolver(GQLRootResolver), Undefined(Undefined))
import qualified Data.Morpheus.Types as M
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.TLS as TLS
import Web.JWT (Signer)
import qualified Web.JWT as JWT
import qualified Web.Scotty as S

import qualified Migration
import Mutation (Mutation)
import qualified Mutation
import Query (Query)
import qualified Query

main :: IO ()
main = do
  manager <- TLS.newTlsManager
  SQLite.withConnection "sqlite.db" $ \conn -> do
    Migration.run conn
    S.scotty 3000 $
      S.post "/api" $ do
        body <- S.body
        response <- Monad.liftIO $ gqlApi conn manager $ LazyByteString.toStrict body
        S.raw $ LazyByteString.fromStrict response

gqlApi :: Connection -> Manager -> ByteString -> IO ByteString
gqlApi conn manager =
  M.interpreter (rootResolver conn manager $ JWT.hmacSecret "Secret")

rootResolver :: Connection -> Manager -> Signer -> GQLRootResolver IO () Query Mutation Undefined
rootResolver conn manager signer =
  GQLRootResolver
    { M.queryResolver = Query.query conn manager signer
    , M.mutationResolver = Mutation.mutation conn
    , M.subscriptionResolver = Undefined
    }
