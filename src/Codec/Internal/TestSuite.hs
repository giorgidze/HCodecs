module Main (main) where

import qualified Codec.Wav as Wav
import qualified Codec.SoundFont as SF
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Test.QuickCheck (quickCheck, (==>))

import Data.Audio
import Codec.Midi
import Codec.ByteString.Parser
import Codec.ByteString.Builder

import Data.Int
import Data.Word
import Data.Bits
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
  quickCheck (prop_audible :: Word8 -> Bool)
  quickCheck (prop_audible :: Word16 -> Bool)
  quickCheck (prop_audible :: Word32 -> Bool)
  -- quickCheck (prop_audible :: Word64 -> Bool)
  quickCheck (prop_audible :: Int8 -> Bool)
  quickCheck (prop_audible :: Int16 -> Bool)
  quickCheck (prop_audible :: Int32 -> Bool)
  -- quickCheck (prop_audible :: Int64 -> Bool)

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

  quickCheck $ roundTrip buildMessage (parseMessage Nothing)
  quickCheck $ roundTrip buildMidi parseMidi
  quickCheck $ \trk -> trk == fromAbsTime (toAbsTime trk :: Track Ticks)
  quickCheck $ \trk td -> trk == fromRealTime td (toRealTime td trk)

  quickCheck $ \m -> (not $ null $ tracks m) ==>
                       let (Midi SingleTrack _ trks) = toSingleTrack m
                       in  length (concat $ tracks m) - length (concat trks) == length (tracks m) - 1

testWav :: IO ()
testWav =  do
  putStrLn "TESTING PARSING AND BUILDING of Wav"
  quickCheck $ roundTrip (Wav.buildWav :: Audio Word8 -> Builder) Wav.parseWav
  quickCheck $ roundTrip (Wav.buildWav :: Audio Int16 -> Builder) Wav.parseWav
  quickCheck $ roundTrip (Wav.buildWav :: Audio Int32 -> Builder) Wav.parseWav

testSoundFont :: IO ()
testSoundFont =  do
  putStrLn "TESTING PARSING AND BUILDING of SoundFont"
  quickCheck $ roundTrip SF.buildSoundFont SF.parseSoundFont

testParserBuilder :: IO ()
testParserBuilder = do
  putStrLn "TESTING PARSING AND BUILDING OF NUMERICAL TYPES"

  quickCheck $ roundTrip putWord8 getWord8
  quickCheck $ roundTrip putWord16be getWord16be
  quickCheck $ roundTrip putWord16le getWord16le
  quickCheck $ \w -> roundTrip putWord24be getWord24be (w .&. 0xFFFFFF)
  quickCheck $ \w -> roundTrip putWord24le getWord24le (w .&. 0xFFFFFF)
  quickCheck $ roundTrip putWord32be getWord32be
  quickCheck $ roundTrip putWord32le getWord32le
  quickCheck $ roundTrip putWord64be getWord64be
  quickCheck $ roundTrip putWord64le getWord64le

  quickCheck $ roundTrip putInt8 getInt8
  quickCheck $ roundTrip putInt16be getInt16be
  quickCheck $ roundTrip putInt16le getInt16le
  quickCheck $ roundTrip putInt32be getInt32be
  quickCheck $ roundTrip putInt32le getInt32le
  quickCheck $ roundTrip putInt64be getInt64be
  quickCheck $ roundTrip putInt64le getInt64le

  quickCheck $ roundTrip putWordHost getWordHost
  quickCheck $ roundTrip putWord16host getWord16host
  quickCheck $ roundTrip putWord32host getWord32host
  quickCheck $ roundTrip putWord64host getWord64host

  quickCheck $ roundTrip putVarLenBe getVarLenBe
  quickCheck $ roundTrip putVarLenLe getVarLenLe

  putStrLn "TESTING PARSING AND BUILDING OF String and ByteString"

  quickCheck $ \s1 s2 -> roundTrip (\s -> putString s `mappend` putString s2) (getString $ length s1) s1
  quickCheck $ \s1 s2 -> roundTrip (\s -> fromByteString s `mappend` fromByteString s2) (getByteString $ B.length s1) s1
  quickCheck $ \s1 s2 -> roundTrip (\s -> fromLazyByteString s `mappend` fromLazyByteString s2) (getLazyByteString $  L.length s1) s1
  quickCheck $ \bs -> roundTrip fromLazyByteString getRemainingLazyByteString bs


main :: IO ()
main = do
  testAudio
  testParserBuilder
  testMidi
  testWav
  testSoundFont
