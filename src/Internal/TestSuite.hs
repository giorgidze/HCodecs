module Main (main) where

import Codec.Midi
import qualified Codec.Wav as Wav
import qualified Codec.SoundFont as SF
import Data.Audio

import Internal.ByteString.Parser
import Internal.ByteString.Builder

import Test.QuickCheck
import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Monoid
import Debug.Trace

roundTrip :: (Eq a, Show a) => (a -> Builder) -> Parser a -> a -> Bool
roundTrip b p a = if Right a == ea'
  then True
  else trace (unlines $ [show ea']) $ False
  where ea' = runParser p bs
        bs = toLazyByteString $ b a

testAudio :: IO ()
testAudio = do
  putStrLn "TESTING Inctances of Audible"
  test (prop_audible :: Word8 -> Bool)
  test (prop_audible :: Word16 -> Bool)
  test (prop_audible :: Word32 -> Bool)
  -- test (prop_audible :: Word64 -> Bool)
  test (prop_audible :: Int8 -> Bool)
  test (prop_audible :: Int16 -> Bool)
  test (prop_audible :: Int32 -> Bool)
  -- test (prop_audible :: Int64 -> Bool)

  -- These two tests are commented, because they fail
  -- Reason for that is the fact that Double is not abble to accomodate
  -- 64 bit numbers in full precision
  where
  prop_audible :: (Eq a, Audible a) => a -> Bool
  prop_audible a = (a ==  fromSample s) && (s >= -1.0) && (s <= 1.0)
    where s = toSample a

testMidi :: IO ()
testMidi =  do
  putStrLn "TESTING PARSING AND BUILDING of Midi"

  test $ roundTrip buildMessage (parseMessage Nothing)
  test $ roundTrip buildMidi parseMidi
  test $ \trk -> trk == fromAbsTime (toAbsTime trk :: Track Ticks)
  test $ \trk td -> trk == fromRealTime td (toRealTime td trk)

  test $ \m -> (not $ null $ tracks m) ==>
               let (Midi SingleTrack _ trks) = toSingleTrack m
               in   length (concat $ tracks m) - length (concat trks) == length (tracks m) - 1

testWav :: IO ()
testWav =  do
  putStrLn "TESTING PARSING AND BUILDING of Wav"
  test $ roundTrip (Wav.buildWav :: Audio Word8 -> Builder) Wav.parseWav
  test $ roundTrip (Wav.buildWav :: Audio Int16 -> Builder) Wav.parseWav
  test $ roundTrip (Wav.buildWav :: Audio Int32 -> Builder) Wav.parseWav

testSoundFont :: IO ()
testSoundFont =  do
  putStrLn "TESTING PARSING AND BUILDING of SoundFont"
  test $ roundTrip SF.buildSoundFont SF.parseSoundFont

testParserBuilder :: IO ()
testParserBuilder = do
  putStrLn "TESTING PARSING AND BUILDING OF NUMERICAL TYPES"

  test $ roundTrip putWord8 getWord8
  test $ roundTrip putWord16be getWord16be
  test $ roundTrip putWord16le getWord16le
  test $ \w -> roundTrip putWord24be getWord24be (w .&. 0xFFFFFF)
  test $ \w -> roundTrip putWord24le getWord24le (w .&. 0xFFFFFF)
  test $ roundTrip putWord32be getWord32be
  test $ roundTrip putWord32le getWord32le
  test $ roundTrip putWord64be getWord64be
  test $ roundTrip putWord64le getWord64le

  test $ roundTrip putInt8 getInt8
  test $ roundTrip putInt16be getInt16be
  test $ roundTrip putInt16le getInt16le
  test $ roundTrip putInt32be getInt32be
  test $ roundTrip putInt32le getInt32le
  test $ roundTrip putInt64be getInt64be
  test $ roundTrip putInt64le getInt64le

  test $ roundTrip putWordHost getWordHost
  test $ roundTrip putWord16host getWord16host
  test $ roundTrip putWord32host getWord32host
  test $ roundTrip putWord64host getWord64host

  test $ roundTrip putVarLenBe getVarLenBe
  test $ roundTrip putVarLenLe getVarLenLe

  putStrLn "TESTING PARSING AND BUILDING OF String and ByteString"

  test $ \s1 s2 -> roundTrip (\s -> putString s `mappend` putString s2) (getString $ length s1) s1
  test $ \s1 s2 -> roundTrip (\s -> fromByteString s `mappend` fromByteString s2) (getByteString $ B.length s1) s1
  test $ \s1 s2 -> roundTrip (\s -> fromLazyByteString s `mappend` fromLazyByteString s2) (getLazyByteString $  L.length s1) s1
  test $ \bs -> roundTrip fromLazyByteString getRemainingLazyByteString bs


main :: IO ()
main = do
  testAudio
  testParserBuilder
  testMidi
  testWav
  testSoundFont
