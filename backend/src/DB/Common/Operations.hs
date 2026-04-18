-- general
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
-- persistent
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}


module DB.Common.Operations
    ( DatabaseIO
    , getAllData
    , getAnd
    , getByAnd
    , getAndIO
    , getByAndIO
    , replaceAndReturn
    , updateF
    , updateByF
    , updateAndReturn
    , updateAndReturnBy
    , programAction
    , programMap
    , programMap2
    , runDatabaseIO
    ) where

-- general
import Control.Monad.Reader
import Control.Monad.Logger
import Data.String.Conversions (cs)
-- persistent
import Database.Persist as DP
import Database.Persist.Sql
import Database.Persist.Sqlite


-- DatabaseIO
type DatabaseIO a = ReaderT SqlBackend IO a


-- data Entity entity =
--  Entity { entityKey :: Key entity
--         , entityVal :: entity }


-- ( m := DatabaseIO )

-- selectList :: (omitted) => [Filter val] -> [Order val] -> m [Entity val]
-- get :: (omitted) => Key val -> m (Maybe val)
-- getBy :: (omitted) => Unique val -> m (Maybe (Entity val))

-- insert :: (omitted) => val -> m (Key val)
-- replace :: (omitted) => Key val -> val -> m ()
-- delete :: (omitted) => Key val -> m ()

getAllData :: PersistRecordBackend v SqlBackend => DatabaseIO [v]
getAllData =
  (entityVal <$>) <$> selectList [] []

getAnd :: PersistRecordBackend v SqlBackend => (DP.Key v -> a -> DatabaseIO o) -> o -> (v -> a) -> DP.Key v -> DatabaseIO o
getAnd dbf init f key =
  maybeM (return init) (dbf key . f) (get key)

getByAnd :: PersistRecordBackend v SqlBackend => (DP.Key v -> a -> DatabaseIO o) -> o -> (v -> a) -> DP.Unique v -> DatabaseIO o
getByAnd dbf init f =
  maybeM (return init) (\ev -> dbf (entityKey ev) (f (entityVal ev))) . getBy

getAndIO :: PersistRecordBackend v SqlBackend => (DP.Key v -> a -> DatabaseIO o) -> o -> (v -> IO a) -> DP.Key v -> DatabaseIO o
getAndIO dbf init f key =
  maybeM (return init) ((=<<) (dbf key) . liftIO . f) (get key)

getByAndIO :: PersistRecordBackend v SqlBackend => (DP.Key v -> a -> DatabaseIO o) -> o -> (v -> IO a) -> DP.Unique v -> DatabaseIO o
getByAndIO dbf init f =
  maybeM (return init) (\ev -> liftIO (f (entityVal ev)) >>= dbf (entityKey ev)) . getBy

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM init f dbf =
  dbf >>= maybe init f


-- replaceAndReturn
replaceAndReturn :: PersistRecordBackend v SqlBackend => DP.Key v -> v -> DatabaseIO v
replaceAndReturn key x = replace key x >> return x


-- update
updateF :: PersistRecordBackend v SqlBackend => (v -> v) -> DP.Key v -> DatabaseIO ()
updateF =
  getAnd replace ()

updateByF :: PersistRecordBackend v SqlBackend => (v -> v) -> DP.Unique v -> DatabaseIO ()
updateByF =
  getByAnd replace ()

updateAndReturn :: PersistRecordBackend v SqlBackend => (v -> v) -> DP.Key v -> DatabaseIO (Maybe v)
updateAndReturn =
  getAnd (\k x -> Just <$> replaceAndReturn k x) Nothing

updateAndReturnBy :: PersistRecordBackend v SqlBackend => (v -> v) -> DP.Unique v -> DatabaseIO (Maybe v)
updateAndReturnBy =
  getByAnd (\k x -> Just <$> replaceAndReturn k x) Nothing


--Program
programAction :: PersistRecordBackend d SqlBackend => (s -> d) -> (d -> s) -> s -> (s -> i -> (s,o)) -> i -> DatabaseIO o
programAction toDB fromDB init response input = do
  dbState <- selectList [] []
  case dbState of
    [] ->
      let (newState,output) = response init input
      in
        do
          insert $ toDB newState
          return output

    eState : _ ->
      let
        stateId = entityKey eState
        state = entityVal eState
        (newState,output) = response (fromDB state) input
      in
        do
          replace stateId $ toDB newState
          return output

programMap :: (s -> i -> (s,o)) -> s -> [i] -> (s,[o])
programMap program state =
  foldr (\i (s,os) -> let (ns,o) = program s i in (ns,o:os)) (state,[])

programMap2 :: (s -> i -> (s,o)) -> s -> (i,i) -> (s,(o,o))
programMap2 program state (i1,i2) =
  let
    (s1,o1) = program state i1
    (s2,o2) = program s1 i2
  in
    (s2,(o1,o2))


--Run
runDatabaseIO :: MonadIO m => FilePath -> Migration -> DatabaseIO a -> m a
runDatabaseIO dbPath migration action =
  liftIO (readyPool dbPath migration >>= runSqlPool action)

readyPool :: FilePath -> Migration -> IO ConnectionPool
readyPool dbPath migration = do
  pool <- mkPool dbPath
  runSqlPool (runMigration migration) pool
  return pool


--ConnectionPool
poolSize :: Int
poolSize = 5

mkPool :: FilePath -> IO ConnectionPool
mkPool filePath =
  runStdoutLoggingT $ createSqlitePool (cs filePath) poolSize



