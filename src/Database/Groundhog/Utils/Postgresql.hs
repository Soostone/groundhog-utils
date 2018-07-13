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
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8          as B
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.Generic.Sql
import           Database.Groundhog.Postgresql
-------------------------------------------------------------------------------
import qualified Database.Groundhog.Utils       as U
-------------------------------------------------------------------------------


pg :: proxy Postgresql
pg = undefined

keyToInt :: PrimitivePersistField (Key a b) => Key a b -> Int
keyToInt = U.keyToInt

keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b -> i
keyToIntegral = U.keyToIntegral

integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i -> Key a b
integralToKey = U.integralToKey

intToKey :: PrimitivePersistField (Key a b) => Int -> Key a b
intToKey = U.intToKey





toEntityPersistValues'
  :: (PersistBackend m, PersistEntity v)
  => v
  -> m [PersistValue]
toEntityPersistValues' v = ($ []) `liftM` toEntityPersistValues v


-------------------------------------------------------------------------------
insertMany
    :: (PersistEntity a, PersistBackend m, PrimitivePersistField (AutoKey a), MonadIO m, MonadBaseControl IO m)
    => [a]
    -> m [AutoKey a]
insertMany vs = do
    vs' <- (concat . map tail) `liftM` mapM toEntityPersistValues' vs
    rawValues <- streamToList =<< queryRaw False query vs' -- rowstream [persistvalue] ~ Acquire (IO (Maybe a))
    return (converter <$> rawValues)
  where
    converter [x] = fromPrimitivePersistValue x
    converter _   = error "Impossible, returning(id) returned cols /= 1"

    query = B.unpack . fromUtf8 $
      "INSERT INTO " <> dbName <> " (" <> fieldNames <> ")" <>
      " VALUES " <> placeholders <> " returning(id)"

    placeholders = commasJoin $ map mkPlace vs

    mkPlace _ = "(" <> commasJoin (map (const "?") fields) <> ")"

    entity = entityDef pg (head vs)
    fieldNames = commasJoin $ map (fromString . fst) fields
    fields = constrParams $ head (constructors entity)
    dbName = mainTableName id entity
