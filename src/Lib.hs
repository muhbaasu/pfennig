{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lib where

import           Control.Exception
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import           Crypto.Scrypt
import           Data.Aeson
import           Data.Int
import           Data.Profunctor.Product         (p3, p4)
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance')
import           Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Data.Time
import           Database.PostgreSQL.Simple
import qualified Opaleye                         as O

connectionString :: ConnectInfo
connectionString = defaultConnectInfo {
  connectPassword = "postgres",
  connectDatabase = "pfennig"}

runQuery :: (Default O.QueryRunner c h) => O.Query c -> IO [h]
runQuery q =
  bracket
  (connect connectionString)
  close
  (\c -> O.runQuery c q)

runAction :: ReaderT Connection IO a -> IO a
runAction act =
  bracket
  (connect connectionString)
  close
  (\c -> runReaderT act c)

data Event' a b c d = Event
  { eventId        :: a
  , eventUserId    :: b
  , eventFields    :: c
  , eventCreatedAt :: d
  }

type Event = Event' Int64 Int64 Value UTCTime
type EventWColumns = Event'
                     (Maybe (O.Column O.PGInt8))
                     (O.Column O.PGInt8)
                     (O.Column O.PGJsonb)
                     (Maybe (O.Column O.PGTimestamptz))
type EventRColumns = Event'
                     (O.Column O.PGInt8)
                     (O.Column O.PGInt8)
                     (O.Column O.PGJsonb)
                     (O.Column O.PGTimestamptz)

makeAdaptorAndInstance' ''Event'

newtype EventId' a = EventId a
makeAdaptorAndInstance' ''EventId'

eventTable :: O.Table
  (Maybe (O.Column O.PGInt8),
   O.Column O.PGInt8,
   O.Column O.PGJsonb,
   Maybe (O.Column O.PGTimestamptz))
  (O.Column O.PGInt8,
   O.Column O.PGInt8,
   O.Column O.PGJsonb,
   O.Column O.PGTimestamptz)
eventTable = O.Table "events" (p4 (O.optional "id",
                                   O.required "user_id",
                                   O.required "fields",
                                   O.optional "created_at"))


eventQuery :: O.Query (O.Column O.PGInt8,
                       O.Column O.PGInt8,
                       O.Column O.PGJsonb,
                       O.Column O.PGTimestamptz)
eventQuery = O.queryTable eventTable

data User' a b c d = User
  { userId           :: a
  , userEmailAddress :: b
  , userPassword     :: c
  , userCreatedAt    :: d
  }

type User = User' Integer T.Text T.Text UTCTime
type UserWColumns = User'
                    (Maybe (O.Column O.PGInt8))
                    (O.Column O.PGText)
                    (O.Column O.PGText)
                    (Maybe (O.Column O.PGTimestamptz))
type UserRColumns = User'
                    (O.Column O.PGInt8)
                    (O.Column O.PGText)
                    (O.Column O.PGText)
                    (O.Column O.PGTimestamptz)

makeAdaptorAndInstance' ''User'

userTable :: O.Table
             (Maybe (O.Column O.PGInt8),
              O.Column O.PGText,
              O.Column O.PGText,
              Maybe (O.Column O.PGTimestamptz))
             (O.Column O.PGInt8,
              O.Column O.PGText,
              O.Column O.PGText,
              O.Column O.PGTimestamptz)
userTable = O.Table "users" (p4 (O.optional "id",
                                 O.required "email_address",
                                 O.required "password",
                                 O.optional "created_at"))

data UserAudit' a b c = UserAudit
  { userAuditId        :: a
  , userAuditUserId    :: b
  , userAuditCreatedAt :: c
  }

type UserAudit = UserAudit' Integer Integer UTCTime
type UserAuditWColumns = UserAudit'
                         (Maybe (O.Column O.PGInt8))
                         (O.Column O.PGInt8)
                         (Maybe (O.Column O.PGInt8))
type UserAuditRColumns = UserAudit'
                         (O.Column O.PGInt8)
                         (O.Column O.PGInt8)
                         (O.Column O.PGInt8)

makeAdaptorAndInstance' ''UserAudit'

userAuditTable :: O.Table
                  (Maybe (O.Column O.PGInt8),
                   O.Column O.PGInt8,
                   Maybe (O.Column O.PGInt8))
                  (O.Column O.PGInt8,
                   O.Column O.PGInt8,
                   O.Column O.PGInt8)
userAuditTable = O.Table "user_audits" (p3 (O.optional "id",
                                            O.required "user_id",
                                            O.optional "created_at"))

addUser :: T.Text -> T.Text -> ReaderT Connection IO [Int64]
addUser mail pass = do
  c <- ask
  p <- liftIO $ encryptPassIO' $ Pass $ TE.encodeUtf8 pass
  let passT = TE.decodeUtf8 $ getEncryptedPass p
  liftIO $ O.runInsertManyReturning c userTable [(Nothing, mailF, O.pgStrictText passT, Nothing)] (\(x, _, _, _) -> x)
  where
    mailF = O.pgStrictText mail
