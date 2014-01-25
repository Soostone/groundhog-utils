{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.Groundhog.Utils.Postgresql where

-------------------------------------------------------------------------------
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
-------------------------------------------------------------------------------
import qualified Database.Groundhog.Utils      as U
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


