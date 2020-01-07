module Effects.User
  ( User
  , create
  , findOne
  , findByEmailAddress
  , list
  , runUserAsMySQL
  ) where

import qualified Control.Exception as Exception
import qualified Data.Time.Clock as Time
import Database.MySQL.Base (MySQLError)
import qualified Database.MySQL.Base as MySQL
import Database.MySQL.Simple (Connection, In(In), Only(Only))
import qualified Database.MySQL.Simple as MySQL
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Prelude (Either, IO, Maybe(Just, Nothing), ($), (<>), (==))
import qualified Prelude

import qualified Migration
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
  -- List all Users by ID
  List :: [Types.UserID] -> User m [Types.User]

Polysemy.makeSem ''User

runUserAsMySQL :: Member (Embed IO) r => Sem (User : r) a -> Sem (Input Connection : r) a
runUserAsMySQL = Polysemy.reinterpret $ \case

  Create name email hashedPassword -> do
    newUser <- Types.newUser name email
    now <- Polysemy.embed Time.getCurrentTime
    conn <- Input.input
    userResult <- Polysemy.embed $
      Exception.tryJust
        (\(e :: MySQLError) ->
          if MySQL.errNumber e == 1062 then
            Just DuplicateEmail
          else
            Nothing
        )
        (MySQL.execute conn query $ params newUser now)
    Prelude.pure $ Prelude.fmap
      (Prelude.const newUser)
      userResult

    where
      query = Prelude.mconcat
        [ "INSERT INTO " <> Migration.usersTable
        , " "
        , "(id, name, email, hashedPassword, createdAt, updatedAt)"
        , " "
        , "VALUES (?, ?, ?, ?, ?, ?)"
        ]
      params newUser now =
        (Types.getID newUser, name, email, hashedPassword, now, now)

  FindOne id -> do
    conn <- Input.input
    maybeUserTuple <- Types.findOne conn query params
    Prelude.pure $ Prelude.fmap Prelude.fst maybeUserTuple

    where
      query = Prelude.mconcat
        [ "SELECT id, name, email, hashedPassword"
        , " "
        , "FROM " <> Migration.usersTable
        , " "
        , "WHERE id = ?"
        ]
      params = Only id

  FindByEmailAddress email -> do
    conn <- Input.input
    Types.findOne conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, name, email, hashedPassword"
        , " "
        , "FROM " <> Migration.usersTable
        , " "
        , "WHERE email = ?"
        ]
      params = Only email

  List userIDs -> do
    conn <- Input.input
    Types.findMany conn query params

    where
      query = Prelude.mconcat
        [ "SELECT id, name, email"
        , " "
        , "FROM " <> Migration.usersTable
        , " "
        , "WHERE id IN ?"
        ]
      params = Only (In userIDs)
