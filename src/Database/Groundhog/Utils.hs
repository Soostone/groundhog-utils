{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Groundhog.Utils
  ( Entity(..)
  , Entity'
  -- * Querying
  , selectEntity
  , selectProducer
  -- * Keys
  , getKey
  , mkKey
  , keyToInt
  , keyToIntegral
  , intToKey
  , integralToKey
  -- * Serialization
  , SC(..)
  , getSC
  , Sh(..)
  , getSh
  ) where

-------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Loops          (whileJust_)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import qualified Data.Acquire                 as Acquire
import           Data.Aeson
import           Data.ByteString.Char8        (ByteString)
import           Data.Conduit
import           Data.Default
import           Data.SafeCopy
import           Data.Serialize
import           Data.Typeable
import           Database.Groundhog           as GH
import           Database.Groundhog.Core      as GH
import           Database.Groundhog.Generic   as GH
import           GHC.Generics
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Data type holding a key and its associated value.  This is used for
-- convenience functions like selectEntity that abstract the common pattern of
-- getting a row and its auto-incremented key.
data Entity k v = Entity
    { entityKey :: k
    , entityVal :: v
    } deriving (Show, Eq, Ord, Generic, Typeable)


-- | The Common case of an entity with a matching key. You almost
-- never need a key that doesn't track the 'entityVal' type.
type Entity' v = Entity (DefaultKey v) v


-------------------------------------------------------------------------------
-- | Convenience wrapper aronud groundhog's 'select' function that also
-- returns keys with each result row.
selectEntity
  :: ( EntityConstr v c
     , Projection' p conn (RestrictionHolder v c) a
     , HasSelectOptions opts conn (RestrictionHolder v c)
     , PersistBackend m
     , Conn m ~ conn
     , Projection p v
     )
  => p
  -> opts
  -> m [Entity (AutoKey v) v]
selectEntity constructor cond = do
    res <- project (AutoKeyField, constructor) cond
    return $ map (uncurry Entity) res


-------------------------------------------------------------------------------
-- | Takes groundhog's streaming-framework-agnostic 'selectStream' to
-- a conduit producer.
selectProducer
  :: forall v c conn m ctor opts b i.
     ( EntityConstr v c
     , HasSelectOptions opts conn (RestrictionHolder v c)
     , PersistBackend m
     , Conn m ~ conn
     , MonadBase IO m
     , Projection ctor b
     , ProjectionDb ctor conn
     , ProjectionRestriction ctor (RestrictionHolder v c)
     , MonadThrow m
     )
  => ctor
  -> opts
  -> ConduitM i (Entity (AutoKey v) b) (ResourceT m) ()
selectProducer ctor opts = do
  rowStream <- fmap (fmap (fmap (uncurry Entity))) <$>
    lift (lift (projectStream (AutoKeyField, ctor) opts))
  rowStreamProducer rowStream


-------------------------------------------------------------------------------
rowStreamProducer
    :: ( MonadIO m
       , MonadThrow m
       , MonadBase IO m
       )
    => RowStream a
    -> ConduitM i a (ResourceT m) ()
rowStreamProducer rowStream = do
  (releaseKey, rowIterator) <- Acquire.allocateAcquire rowStream
  whileJust_ (liftIO rowIterator) yield
  release releaseKey


-------------------------------------------------------------------------------

-- | Pull the Int out of a db AutoKey.
getKey :: (SinglePersistField a, PersistBackend m) => a -> m Int
getKey k = toSinglePersistValue k >>= fromSinglePersistValue


-------------------------------------------------------------------------------
mkKey :: (PersistBackend m, SinglePersistField a, SinglePersistField b) => a -> m b
mkKey k = toSinglePersistValue k >>= fromSinglePersistValue


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue . toPrimitivePersistValue


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue . toPrimitivePersistValue


-- | SafeCopy PrimitivePersistField wrapper. Anything you stuff in
-- here will be persisted in database as a SafeCopy blob.
newtype SC a = SC { _getSC :: a }
  deriving (Eq,Show,Read,Ord,Generic,Typeable,ToJSON,FromJSON)
makeLenses ''SC
makeWrapped ''SC

instance NeverNull (SC a)

instance SafeCopy a => PersistField (SC a) where
    persistName _ = "SC" ++ delim : delim : persistName (undefined :: ByteString)
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbBlob False Nothing Nothing

instance SafeCopy a => PrimitivePersistField (SC a) where
    toPrimitivePersistValue (SC a) = toPrimitivePersistValue $ runPut $ safePut a
    fromPrimitivePersistValue x =
      either (error "SafeCopy failed in SC wrapper.") SC $
        runGet safeGet (fromPrimitivePersistValue x)


-- | Show PrimitivePersistField wrapper. Wrap your data into this and
-- it will be marshalled to groundhog via its read/show instances.
newtype Sh a = Sh { _getSh :: a }
  deriving (Eq,Show,Read,Ord,Generic,Typeable,Default,ToJSON,FromJSON)
makeLenses ''Sh
makeWrapped ''Sh

instance NeverNull (Sh a)

instance (Show a, Read a) => PersistField (Sh a) where
    persistName _ = "Sh" ++ delim : delim : persistName (undefined :: ByteString)
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance (Show a, Read a) => PrimitivePersistField (Sh a) where
    toPrimitivePersistValue (Sh a) = toPrimitivePersistValue $ show a
    fromPrimitivePersistValue x = Sh $ read (fromPrimitivePersistValue x)
