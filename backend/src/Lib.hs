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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( startApp
    , generate
    ) where

import Data.Aeson
import Servant
import Servant.Elm
import Elm
import Elm.Derive as ED
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

import Backend
import Generator


port :: Int
port = 8080

dbPath :: FilePath
dbPath = "sqlite.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Memo
  title   String
  content String
  deriving Generic
|]

deriveBoth ED.defaultOptions ''Memo


type API = "allMemos" :> Get '[JSON] [Memo]
           :<|> "postMemo" :> ReqBody '[JSON] Memo :> Post '[JSON] Memo

server :: ConnectionPool -> Server API
server pool = do
  dbAction pool getAll :<|> dbAction pool . insertMemo

getAll :: DatabaseIO [Memo]
getAll = do
  allMemos <- selectList [] []
  return $ map entityVal allMemos

insertMemo :: Memo -> DatabaseIO Memo
insertMemo memo = do
  memoId <- insert memo
  return memo

startApp :: IO ()
startApp = do
  pool <- readyPool dbPath migrateAll
  runServerWithCors port (Proxy :: Proxy API) server pool

generate :: IO ()
generate =
  generateElm port (Proxy :: Proxy Memo) (Proxy :: Proxy API) "Api"