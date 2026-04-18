{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}


module DB.Message
  ( Message
  , getAllMessage
  , postMessage
  )
where

import GHC.Generics
import Control.Monad.Reader
import Data.Aeson
import Database.Persist
import Database.Persist.Sql as DPS
import Database.Persist.TH
import Elm.Derive as ED

import DB.Common

-- DB file
dbPath :: FilePath
dbPath = "Message.db"


-- Message
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
  title   String
  content String
  deriving Generic
|]
deriveElmDef ED.defaultOptions ''Message

instance ToJSON Message
instance FromJSON Message


-- IO
getAllMessage :: MonadIO m => m [Message]
getAllMessage = runDB dbGetAllMessage

postMessage :: MonadIO m => Message -> m ()
postMessage = runDB . dbPostMessage


-- DatabaseIO
dbGetAllMessage :: DatabaseIO [Message]
dbGetAllMessage = getAllData

dbPostMessage :: Message -> DatabaseIO ()
dbPostMessage c = insert c >> return ()


-- runDB
runDB :: MonadIO m => DatabaseIO a -> m a
runDB = runDatabaseIO dbPath migrateAll
