-----------------------------------------------------------------------------
-- |
-- Module      : Codec.Wav
-- Copyright   : George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : Experimental
-- Portability : Portable
--
-- Module for reading and writting of WAVE (.wav) audio files.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Codec.Wav (
    importFile
  , exportFile
  , parseWav
  , buildWav
  , AudibleInWav(..)
  ) where

import Data.Audio
import Codec.ByteString.Parser
import Codec.ByteString.Builder

import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Array.Unboxed
import Data.Array.IO
import Data.Bits


import Control.Monad
import Control.Applicative

class AudibleInWav a where
  parseSample :: Parser a
  buildSample :: a -> Builder
  bitsPerSample :: a -> Int

instance AudibleInWav Word8 where
  parseSample = getWord8
  buildSample = putWord8
  bitsPerSample _ = 8

instance AudibleInWav Int16 where
  parseSample = getInt16le
  buildSample = putInt16le
  bitsPerSample _ = 16

instance AudibleInWav Int32 where
  parseSample = getInt32le
  buildSample = putInt32le
  bitsPerSample _ = 32

instance AudibleInWav Int64 where
  parseSample = getInt64le
  buildSample = putInt64le
  bitsPerSample _ = 64

parserSelector :: (Audible a, AudibleInWav a) => Int -> Parser a
parserSelector 8  = (parseSample :: Parser Word8) >>= return . fromSample . toSample
parserSelector 16 = (parseSample :: Parser Int16) >>= return . fromSample . toSample
parserSelector 24 = ((getWord24le >>= return . fromIntegral . (flip shiftL) 8) :: Parser Int32) >>= return . fromSample . toSample
parserSelector 32 = (parseSample :: Parser Int32) >>= return . fromSample . toSample
parserSelector 64 = (parseSample :: Parser Int64) >>= return . fromSample . toSample
parserSelector n  = fail $ show n ++ " bitsPerSample is not supported"

bytesPerSample :: (AudibleInWav a) => a -> Int
bytesPerSample a = div (bitsPerSample a) 8 

importFile :: (MArray IOUArray a IO, IArray UArray a, Audible a, AudibleInWav a) => FilePath -> IO (Either String (Audio a))
importFile n = do
  bs <- L.readFile n
  return $! runParser parseWav bs

exportFile :: (IArray UArray a, Audible a, AudibleInWav a) => FilePath -> Audio a ->  IO ()
exportFile f w = L.writeFile f (toLazyByteString $ buildWav w)

-- All numerical values are stored in little endian format
--
parseWav :: (MArray IOUArray a IO, IArray UArray a, Audible a, AudibleInWav a) => Parser (Audio a)
parseWav = do
  _ <- string "RIFF"
--  n <- remaining
--  expect (\w -> fromIntegral w ==  n - 4) getWord32le
  _ <- getWord32le -- chunkSize
  _ <- string "WAVE"
  _ <- many parseUnknownChunk
  (sampleRate1,channelNumber1,bitsPerSample1) <- parseFmt
  _ <- many parseUnknownChunk
  sampleData1 <- parseData channelNumber1 bitsPerSample1
  return $! (Audio sampleRate1 channelNumber1 sampleData1)

buildWav :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
buildWav a = mconcat [
    putString "RIFF"
  , putWord32le $ fromIntegral chunkSize
  , putString "WAVE"
  , buildFmt a
  , buildData a]
  where
  sd = sampleData a
  chunkSize =
      4  -- "WAVE" 
    + 24 -- fmt chunk
    + 8  -- data chunk header  
    + (fromIntegral $ sampleNumber sd) * (bytesPerSample $ sampleType sd)
       -- sample data

parseFmt :: Parser (Int,Int,Int)
parseFmt = do
  _ <- string "fmt "
  chunkSize <- getWord32le >>= return . fromIntegral
  _ <- word16le 1 -- compression code
  channelNumber1 <- getWord16le >>= return . fromIntegral
  sampleRate1 <- getWord32le >>= return . fromIntegral
  avgBytesPerSec <- getWord32le >>= return . fromIntegral
  bytesPerSampleSlice <- getWord16le >>= return . fromIntegral
  when (avgBytesPerSec /= sampleRate1 * bytesPerSampleSlice) $ 
    fail "avgBytesPerSec /= sampleRate * bytesPerSampleSlise"
  bitsPerSample1 <- expect (\w -> (mod w 8 == 0) && w <= 64) getWord16le >>= return . fromIntegral
  when (bytesPerSampleSlice /= (div bitsPerSample1 8) * channelNumber1) $ 
    fail "bytesPerSampleSlice /= (div bitsPerSample 8) * channelNumber"
  skip (chunkSize - 16) -- skip extra fromat bytes
  return $! (sampleRate1,channelNumber1,bitsPerSample1)

buildFmt :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
buildFmt a = mconcat [
    putString   $ "fmt "
  , putWord32le $ 16 -- chunk size
  , putWord16le $ 1  -- compression code
  , putWord16le $ fromIntegral $ channelNumber a
  , putWord32le $ fromIntegral $ sampleRate a
  , putWord32le $ fromIntegral $ avgBytesPerSec
  , putWord16le $ fromIntegral $ bytesPerSampleSlice
  , putWord16le $ fromIntegral $ bitsPS
  ]
  where
  sd = sampleData a
  bitsPS = bitsPerSample $ sampleType sd
  bytesPS = bytesPerSample $ sampleType sd
  bytesPerSampleSlice = bytesPS * channelNumber a
  avgBytesPerSec = sampleRate a * bytesPerSampleSlice
  
parseData :: (MArray IOUArray a IO, IArray UArray a, Audible a, AudibleInWav a)
  => Int -> Int -> Parser (SampleData a)
parseData cn bitsPS = do
  _ <- string "data"
  let bytesPS = div bitsPS 8
  chunkSize <- expect (\w -> mod (fromIntegral w) bytesPS == 0) getWord32le
               >>= return . fromIntegral
  let sn = fromIntegral $ div chunkSize bytesPS
  when (mod sn (fromIntegral cn) /= 0) $ fail "mod sampelNumber channelNumber /= 0)"
  parseSampleData sn (parserSelector bitsPS) 
 
buildData :: (IArray UArray a, Audible a, AudibleInWav a) => Audio a -> Builder
buildData a = mconcat [
    putString "data"
  , putWord32le $ fromIntegral $ chunkSize
  , buildSampleData buildSample sd]
  where
  sd = sampleData a
  chunkSize = (fromIntegral $ sampleNumber sd) * (bytesPerSample $ sampleType sd)

parseUnknownChunk :: Parser ()
parseUnknownChunk = do
  _ <- expect (\s -> s /= "data" && s /= "fmt ") (getString 4)
  chunkSize <- getWord32le
  skip(fromIntegral chunkSize)
  return ()
