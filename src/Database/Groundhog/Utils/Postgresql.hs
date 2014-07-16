{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Database.Groundhog.Utils.Postgresql
    ( pg
    , keyToInt
    , intToKey
    , keyToIntegral
    , integralToKey
    , insertMany

    , U.getKey
    , U.mkKey
    , U.SC (..)
    , U.Sh (..)

    ) where

-------------------------------------------------------------------------------
import           Control.Monad
import qualified Data.ByteString.Char8          as B
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.Generic.Sql
import           Database.Groundhog.Postgresql
-------------------------------------------------------------------------------
import qualified Database.Groundhog.Utils       as U
-------------------------------------------------------------------------------


pg :: Proxy Postgresql
pg = undefined

keyToInt :: PrimitivePersistField (Key a b) => Key a b -> Int
keyToInt = U.keyToInt pg

keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b -> i
keyToIntegral = U.keyToIntegral pg

integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i -> Key a b
integralToKey = U.integralToKey pg

intToKey :: PrimitivePersistField (Key a b) => Int -> Key a b
intToKey = U.intToKey pg





toEntityPersistValues' v = ($ []) `liftM` toEntityPersistValues v


-------------------------------------------------------------------------------
insertMany
    :: (PersistEntity a, PersistBackend m, PrimitivePersistField (AutoKey a))
    => [a]
    -> m [AutoKey a]
insertMany vs = do
    vs' <- (concat . map tail) `liftM` mapM toEntityPersistValues' vs
    queryRaw False query vs' (mapAllRows converter)
  where
    converter [x] = return $ fromPrimitivePersistValue pg x

    query = B.unpack . fromUtf8 $
      "INSERT INTO " <> dbName <> " (" <> fieldNames <> ")" <>
      " VALUES " <> placeholders <> " returning(id)"

    placeholders = commasJoin $ map mkPlace vs

    mkPlace _ = "(" <> commasJoin (map (const "?") fields) <> ")"

    entity = entityDef pg (head vs)
    fieldNames = commasJoin $ map (fromString . fst) fields
    fields = constrParams $ head (constructors entity)
    dbName = mainTableName id entity


