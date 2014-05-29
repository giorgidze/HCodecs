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

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Internal.Arbitrary (
    arrayGen
  , stringNulGen
  ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Test.QuickCheck
import Data.Array.IArray
import Data.List
import Data.Word
import Data.Char

instance Arbitrary L.ByteString where
  arbitrary = arbitrary >>= return . L.fromChunks . filter (not. B.null)

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary
  
instance (Arbitrary e, Num i, IArray Array e, Ix i) =>  Arbitrary (Array i e) where
  arbitrary = do
    n <- choose (1, 128)
    arrayGen n

arrayGen :: (Arbitrary e, Num i, IArray a e, Ix i) => Word -> Gen (a i e)
arrayGen 0 = error "Array with 0 elements can not be defined"
arrayGen n = do
  es <- vector (fromIntegral n)
  return $! listArray (0 , fromIntegral $ n - 1) es
  
stringNulGen :: Word -> Gen String
stringNulGen n = do
  sequence $ genericReplicate n $ choose (1,255) >>= return . chr
