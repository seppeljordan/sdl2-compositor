{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Cache.LRU
import Control.Concurrent.STM
import Distribution.TestSuite as TestSuite
import Test.Hspec as Hspec
import Test.Hspec.Core.Runner
import Test.QuickCheck
import Test.QuickCheck.Monadic as QC

import SDL.Data.Cache

main :: IO ()
main = hspec functionTest

newtype CacheInteger = CacheInteger { fromCacheInteger :: Integer }
                     deriving (Eq,Ord,Num,Arbitrary,Show)

instance Cacheable CacheInteger where
  releaseResource _ = return ()

data StatefulBool a = StatefulBool { fromStatefulBool :: TVar Bool
                                   , sbVal :: a
                                   }

instance Cacheable (StatefulBool a) where
  releaseResource b = atomically $ writeTVar (fromStatefulBool b) True

isReleased :: StatefulBool a -> IO Bool
isReleased b = atomically $ readTVar (fromStatefulBool b)

newStatefulBool :: a -> IO (StatefulBool a)
newStatefulBool x = StatefulBool <$> (atomically $ newTVar False) <*> pure x

functionTest :: Spec
functionTest =
  describe "Data.Cache.LRU" $ do
  it "drops the least recently added item" $ do
    property $ \ (Positive (cacheSize :: Int)) ->
      snd $
      foldl
      (\ (lru,ok) n ->
        if not ok
        then (lru, False)
        else case insertInforming n n lru of
               (lru', Nothing) -> (lru', True)
               (lru', Just (m,_)) ->
                 if m == n - cacheSize
                 then (lru', True)
                 else (lru', False)
      )
      (newLRU (Just (fromIntegral cacheSize)), True)
      [1..cacheSize * 2]

  describe "SDL.Data.Cache.Cache" $ do
    it "produces the same result as without the Cache" $
      property $ \ (n :: CacheInteger) ->
      monadicIO $ assert =<< QC.run
      (do let calc x = return (2*x)
          res <- calc n
          cache <- newCache 1
          resCached <- throughCache cache n (calc n)
          return (res == resCached)
      )
    it "releases all resources when emptying the cache" $
      property $ \ (Positive (cacheSize :: Int)) (Positive (differentElements :: Int)) ->
      monadicIO $ assert =<< QC.run
      (do cache <- newCache cacheSize
          bs <- mapM
                (\n -> throughCache cache n (newStatefulBool n))
                [1..differentElements]
          emptyCache cache
          and <$> mapM isReleased bs
      )
