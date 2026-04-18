
module DB.Common.PersistField where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Sql

import Data.Text as T


-- Either

instance (PersistField a, PersistField b) => PersistField (Either a b) where
  toPersistValue (Left a) = PersistList [PersistBool True, toPersistValue a]
  toPersistValue (Right b) = PersistList [PersistBool False, toPersistValue b]
  fromPersistValue v =
    case fromPersistValue v of
      Right [PersistBool True,va] -> Left <$> fromPersistValue va
      Right [PersistBool False,vb] -> Right <$> fromPersistValue vb
      Left e -> Left e
      _ -> Left $ T.pack $ "Expercted 2 item PersistList, received: " ++ show v

instance (PersistFieldSql a, PersistFieldSql b) => PersistFieldSql (Either a b) where
  sqlType _ = SqlString


-- Tree
{-
instance PersistField a => PersistField (Tree a) where
  toPersistValue (Node x t) =
    PersistList [toPersistValue x,toPersistValue t]
  fromPersistValue v =
    case fromPersistValue v of
      Right [a,t] -> Node <$> fromPersistValue a <*> fromPersistValue t
      Left e -> Left e
      _ -> Left $ T.pack $ "Expected PersistList [_,_], received: " ++ show v

instance PersistFieldSql a => PersistFieldSql (Tree a) where
  sqlType _ = SqlString

-}
