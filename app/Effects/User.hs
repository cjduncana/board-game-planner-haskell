module Effects.User
  ( User
  , create
  , findOne
  , findByEmailAddress
  , runUserAsSQLite
  ) where

import qualified Control.Exception as Exception
import qualified Data.Time.Clock as Time
import Database.SQLite.Simple
    (Connection, Error(ErrorConstraint), NamedParam((:=)), Query, SQLError)
import qualified Database.SQLite.Simple as SQLite
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (Either, IO, Maybe(Just, Nothing), ($), (<>))
import qualified Prelude

import Types.EmailAddress (EmailAddress)
import Types.HashedPassword (HashedPassword)
import Types.NonEmptyText (NonEmptyText)
import qualified Types.User as Types

data CreateError
  = DuplicateEmail

data User m a where
  -- Create a new User
  Create :: NonEmptyText -> EmailAddress -> HashedPassword -> User m (Either CreateError Types.User)
  -- Find a User by their ID
  FindOne :: Types.UserID -> User m (Maybe Types.User)
  -- Find a User by their email address
  FindByEmailAddress :: EmailAddress -> User m (Maybe (Types.User, HashedPassword))

Polysemy.makeSem ''User

runUserAsSQLite :: Member (Embed IO) r => Sem (User : r) a -> Sem (Input Connection : r) a
runUserAsSQLite = Polysemy.reinterpret $ \case

  Create name email hashedPassword -> do
    newUser <- Types.newUser name email
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    userResult <- Polysemy.embed $
      Exception.tryJust
        (\(e :: SQLError) ->
          case SQLite.sqlError e of
            ErrorConstraint -> Just DuplicateEmail
            _ -> Nothing
          )
        (SQLite.executeNamed conn query $ params newUser now)
    Prelude.pure $ Prelude.fmap
      (Prelude.const newUser)
      userResult

    where
      query = Prelude.mconcat
        [ "INSERT INTO " <> usersTable
        , " "
        , "(id, name, email, hashedPassword, createdAt, updatedAt)"
        , " "
        , "VALUES (:id, :name, :email, :hashedPassword, :createdAt, :updatedAt)"
        ]
      params newUser now =
        [ ":id" := Types.getID newUser
        , ":name" := name
        , ":email" := email
        , ":hashedPassword" := hashedPassword
        , ":createdAt" := now
        , ":updatedAt" := now
        ]

  FindOne id -> do
    conn <- Input.input
    maybeUserTuple <- Types.findOne conn query params
    Prelude.pure $ Prelude.fmap Prelude.fst maybeUserTuple

    where
      query = Prelude.mconcat
        [ "SELECT id, name, email"
        , " "
        , "FROM " <> usersTable
        , " "
        , "WHERE id = :id"
        ]
      params = [ ":id" := id ]

  FindByEmailAddress email -> do
    conn <- Input.input
    Types.findOne conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, name, email, hashedPassword"
        , " "
        , "FROM " <> usersTable
        , " "
        , "WHERE email = :email"
        ]
      params = [ ":email" := email ]

usersTable :: Query
usersTable = "users"
