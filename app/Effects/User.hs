module Effects.User
  ( User
  , create
  , find
  , findByEmailAddress
  , runUserAsSQLite
  ) where

import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple
    (Connection, Error(ErrorConstraint), NamedParam((:=)), Query, SQLError)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude
    (Either, IO, Maybe(Just, Nothing), mconcat, pure, ($), (<$), (<$>), (<>))

import Types.EmailAddress (EmailAddress)
import Types.HashedPassword (HashedPassword)
import Types.NonEmptyText (NonEmptyText)
import qualified Types.User as Types
import Types.UUID (UUID)
import qualified Types.UUID as UUID

data CreateError
  = DuplicateEmail

data User m a where
  -- Create a new User
  Create :: NonEmptyText -> EmailAddress -> HashedPassword -> User m (Either CreateError Types.User)
  -- Find a User by their ID
  Find :: UUID -> User m (Maybe Types.UserTuple)
  -- Find a User by their email address
  FindByEmailAddress :: EmailAddress -> User m (Maybe Types.UserTuple)

Polysemy.makeSem ''User

runUserAsSQLite :: Member (Embed IO) r => Sem (User : r) a -> Sem (Input Connection : r) a
runUserAsSQLite = Polysemy.reinterpret $ \case

  Create name email hashedPassword -> do
    id <- UUID.randomUUID
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    let query = mconcat [ "INSERT INTO " <> usersTable
                        , " "
                        , "(id, name, email, hashedPassword, createdAt, updatedAt)"
                        , " "
                        , "VALUES (:id, :name, :email, :hashedPassword, :createdAt, :updatedAt)"
                        ]
    let params = [ ":id" := id
                 , ":name" := name
                 , ":email" := email
                 , ":hashedPassword" := hashedPassword
                 , ":createdAt" := now
                 , ":updatedAt" := now
                 ]
    userResult <- Polysemy.embed $
      Exception.tryJust
        (\(e :: SQLError) ->
          case SQLite.sqlError e of
            ErrorConstraint -> Just DuplicateEmail
            _ -> Nothing
          )
        (SQLite.executeNamed conn query params)
    pure $ Types.create id name email <$ userResult

  Find id -> do
    conn <- Input.input
    let query = mconcat [ "SELECT id, name, email, hashedPassword"
                        , " "
                        , "FROM " <> usersTable
                        , " "
                        , "WHERE id = :id"
                        ]
    let params = [ ":id" := id ]
    Polysemy.embed $
      Maybe.listToMaybe <$> SQLite.queryNamed conn query params

  FindByEmailAddress email -> do
    conn <- Input.input
    let query = mconcat [ "SELECT id, name, email, hashedPassword"
                        , " "
                        , "FROM " <> usersTable
                        , " "
                        , "WHERE email = :email"
                        ]
    let params = [ ":email" := email ]
    Polysemy.embed $
      Maybe.listToMaybe <$> SQLite.queryNamed conn query params

usersTable :: Query
usersTable = "users"
