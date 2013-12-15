-----------------------------------------------------------------------------
-- |
-- Module      : Data.Audio
-- Copyright   : George Giorgidze
-- License     : BSD3
--
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : Experimental
-- Portability : Portable
--
-- General purpose data type for representing an audio data.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Audio (
   Sample
 , Audio (..)
 , SampleData
 , SampleMode(..)
 , sampleType
 , sampleNumber
 , convert
 , parseSampleData
 , buildSampleData
 , Audible
 , toSample
 , fromSample
 ) where

import Data.Arbitrary
import Data.ByteString.Parser
import Data.ByteString.Builder

import Test.QuickCheck

import Data.Word
import Data.Int

import Data.Array.Unboxed
import Data.Array.IO

import Data.Monoid

import System.IO.Unsafe


type Sample = Double
type SampleData a = UArray Int a

class Audible a where
  toSample :: a -> Sample
  fromSample :: Sample -> a

-- It is required that sampleNummer `mod` channelNumber == 0
data Audio a = Audio {
    sampleRate :: Int
  , channelNumber :: Int
  , sampleData :: SampleData a
  }

instance (Eq a, IArray UArray a) => Eq (Audio a) where
  a1 == a2 = and [
      sampleRate a1 == sampleRate a2
    , channelNumber a1 == channelNumber a2
    , assocs (sampleData a1) == assocs (sampleData a2)]


instance (Show a, IArray UArray a) => Show (Audio a) where
  show a = "Sample Rate: " ++ (show $ sampleRate a) ++ "\n" ++
           "Channel Number: " ++ (show $ channelNumber a) ++ "\n" ++
           "Sample Data Array Bounds: " ++ (show $ bounds $ sampleData a)


instance (Arbitrary a, IArray UArray a) => Arbitrary (Audio a) where
  arbitrary = do
    sr <- choose (1, 44100 * 8)
    cn <- choose (1, 64)
    sn <- choose (1, 1024) >>= return . (fromIntegral cn *)
    sd <- arrayGen sn
    return (Audio sr cn sd)

sampleNumber :: (IArray UArray a) => SampleData a -> Int
sampleNumber sd = (snd (bounds sd)) + 1

sampleType :: (IArray UArray a) => SampleData a -> a
sampleType sd = undefined `asTypeOf` (sd ! 0)

convert :: (Audible a, Audible b, IArray UArray a, IArray UArray b) => SampleData a -> SampleData b
convert sd = amap (fromSample . toSample) sd

parseSampleData :: (MArray IOUArray a IO, IArray UArray a) => Int -> Parser a -> Parser (SampleData a)
parseSampleData sn p = pAux 0 unsafeNewArray
  where
  pAux i acc | (i == sn) = seq i $ seq acc $ return acc
  pAux i acc | otherwise = seq i $ seq acc $ do
    s <- p
    seq s (pAux (i + 1) (unsafeWriteArray acc i s))

  unsafeFreezeAux :: (MArray IOUArray a IO, IArray UArray a) => IOUArray Int a -> IO (SampleData a)
  unsafeFreezeAux = unsafeFreeze

  unsafeNewArray         = unsafePerformIO (newArray_ (0, sn - 1) >>= unsafeFreezeAux )
  unsafeWriteArray a i e = unsafePerformIO (do a' <- unsafeThaw a; writeArray a' i e; unsafeFreezeAux a';)


buildSampleData :: (IArray UArray a) => (a -> Builder) -> SampleData a -> Builder
buildSampleData b = mconcat . map b . elems

instance Audible Int8 where
  toSample a = (fromIntegral a) / (2 ** 7)
  fromSample s = round $ s * (2 ** 7)
instance Audible Int16 where
  toSample a = (fromIntegral a) / (2 ** 15)
  fromSample s = round $ s * (2 ** 15)
instance Audible Int32 where
  toSample a = (fromIntegral a) / (2 ** 31)
  fromSample s = round $ s * (2 ** 31)
instance Audible Int64 where
  toSample a = (fromIntegral a) / (2 ** 63)
  fromSample s = round $ s * (2 ** 63)
instance Audible Word8 where
  toSample a = (fromIntegral a) / (2 ** 7) - 1.0
  fromSample s = round $ (s + 1.0) * (2 ** 7)
instance Audible Word16 where
  toSample a = (fromIntegral a) / (2 ** 15) - 1.0
  fromSample s = round $ (s + 1.0) * (2 ** 15)
instance Audible Word32 where
  toSample a = (fromIntegral a) / (2 ** 31) - 1.0
  fromSample s = round $ (s + 1.0) * (2 ** 31)
instance Audible Word64 where
  toSample a = (fromIntegral a) / (2 ** 63) - 1.0
  fromSample s = round $ (s + 1.0) * (2 ** 63)

instance Audible Float where
  toSample = realToFrac
  fromSample = realToFrac

instance Audible Double where
  toSample = id
  fromSample = id

data SampleMode = NoLoop | ContLoop | PressLoop deriving (Eq, Show)

instance Arbitrary SampleMode where
  arbitrary = oneof [return NoLoop, return ContLoop, return PressLoop]
