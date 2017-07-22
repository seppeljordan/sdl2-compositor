{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides a simple caching implementation based on the
-- LRU caching strategy.  The cache is implemented via software
-- transactional memory, which means that you can use the cache from
-- different threads.
module SDL.Data.Cache
    (
      Cacheable (..)
    , Cache
    , newCache
    , throughCache
    , emptyCache
    )
where

import           Data.Cache.LRU (insertInforming, newLRU, maxSize, toList)
import           Data.Cache.LRU.IO (newAtomicLRU,lookup)
import           Data.Cache.LRU.IO.Internal (AtomicLRU(C),modifyMVar')
import           Prelude hiding (lookup)
import qualified SDL as SDL (Texture, destroyTexture, Surface,freeSurface)
import qualified SDL.Raw as Raw

import           SDL.Data.Texture

-- | Something is cacheable if there is an action to release the
-- resource (if necessary).
class Cacheable r where
  releaseResource :: r -> IO ()

instance Cacheable (SDL.Texture) where
  releaseResource = SDL.destroyTexture

instance Cacheable (SDL.Surface) where
  releaseResource = SDL.freeSurface

instance Cacheable RawTexture where
  releaseResource (RawTexture tex) = Raw.destroyTexture tex

instance (Cacheable a) => Cacheable (Maybe a) where
  releaseResource Nothing = return ()
  releaseResource (Just r) = releaseResource r

instance Cacheable Int where
  releaseResource _ = return ()

instance (Cacheable a) => Cacheable [a] where
  releaseResource = mapM_ releaseResource

-- | Thread safe LRU cache.
newtype Cache k a = Cache (AtomicLRU k a)

-- | Create a new cache instance.
newCache :: (Ord k) =>
            Int -- ^ the size of the cache to be created (in elements)
         -> IO (Cache k a)
newCache s = Cache <$>
             newAtomicLRU (Just . fromIntegral $ s)

putInCache :: forall k r .
              (Ord k, Cacheable r) => Cache k r -> k -> IO r -> IO r
putInCache (Cache (C c)) key action = do
  newResource <- action
  mOldResource <- modifyMVar' c (return . insertInforming key newResource)
  mapM_ (releaseResource.snd) mOldResource
  pure newResource

lookupFromCache :: (Ord k) => Cache k r -> k -> IO (Maybe r)
lookupFromCache (Cache var) key =
  lookup key var

-- | Check if a certain element is already cached.  If not execute the
-- action the generate this element.
throughCache :: (Cacheable r, Ord k) =>
                Cache k r -- ^ cache instance
             -> k         -- ^ cache key
             -> IO r      -- ^ action to generate the resource
             -> IO r
throughCache cache key action = do
  mVal <- lookupFromCache cache key
  case mVal of
   Nothing -> putInCache cache key action
   Just val -> return val

-- | Invalidate every element in the cache and release the resources
-- accordingly.
emptyCache :: (Cacheable r, Ord k) => Cache k r -> IO ()
emptyCache (Cache (C c)) = do
  res <- modifyMVar' c $ \ lru ->
    return (newLRU (maxSize lru), map snd . Data.Cache.LRU.toList $ lru)
  mapM_ releaseResource res
