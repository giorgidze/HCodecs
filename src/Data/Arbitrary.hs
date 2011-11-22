-----------------------------------------------------------------------------
-- |
-- Module      : Data.Arbitrary
-- Copyright   : George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : Experimental
-- Portability : Portable
--
-- Additional instances of the 'Arbitrary' type class defined in 'Test.QuickCheck'.
-- Some portions of code were copied from the test suite of 'binary' package.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Arbitrary (
    arrayGen
  , stringNulGen
  ) where


import Test.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Word
import Data.Int
import Data.Char

import Data.List
import Data.Array.IArray

-- random since 1.0.1.0 defines the following instances
#if MIN_VERSION_random(1,0,1)

import System.Random ()

#else

import System.Random (RandomGen,Random,random,randomR)

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,fromIntegral b :: Integer) g of
                             (x1,g1) -> (fromIntegral x1, g1)
#endif

instance Arbitrary Word8 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word16 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word32 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word64 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int8 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int16 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int32 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Int64 where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Word where
    arbitrary       = choose (minBound, maxBound)
    coarbitrary     = undefined

instance Arbitrary Char where
    arbitrary = choose (0, 255) >>= return . toEnum
    coarbitrary = undefined

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.fromChunks . filter (not. B.null)
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (B.unpack s)
  
instance (Arbitrary e, Num i, IArray Array e, Ix i) =>  Arbitrary (Array i e) where
  arbitrary = do
    n <- choose (1, 128)
    arrayGen n
  coarbitrary = undefined

arrayGen :: (Arbitrary e, Num i, IArray a e, Ix i) => Word -> Gen (a i e)  
arrayGen 0 = error "Array with 0 elements can not be defined"
arrayGen n = do 
  es <- vector (fromIntegral n)
  return $! listArray (0 , fromIntegral $ n - 1) es
  
stringNulGen :: Word -> Gen String
stringNulGen n = do
  sequence $ genericReplicate n $ choose (1,255) >>= return . chr