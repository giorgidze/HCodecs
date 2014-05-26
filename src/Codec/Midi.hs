-----------------------------------------------------------------------------
-- |
-- Module      : Codec.Midi
-- Copyright   : George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : Experimental
-- Portability : Portable
--
-- Reading, writing and maniplating of standard MIDI files
--
-----------------------------------------------------------------------------

module Codec.Midi 
  (
    Midi (..)
  , FileType (..)
  , Track
  , TimeDiv (..)
  , Message (..)
  
  , Ticks
  , Time
  , Channel
  , Key
  , Velocity
  , Pressure
  , Preset
  , Bank
  , PitchWheel
  , Tempo 
  
  , isNoteOff
  , isNoteOn
  , isKeyPressure
  , isControlChange
  , isProgramChange
  , isChannelPressure
  , isPitchWheel
  , isChannelMessage
  , isMetaMessage
  , isSysexMessage
  , isTrackEnd
  
  , removeTrackEnds
  , toSingleTrack
  , merge
  , fromAbsTime
  , toAbsTime
  , toRealTime
  , fromRealTime
  
  , importFile
  , exportFile
  , parseMidi
  , buildMidi
  , parseTrack
  , buildTrack
  , parseMessage
  , buildMessage
  )
   where

import Internal.ByteString.Parser
import Internal.ByteString.Builder
import Internal.Arbitrary ()

import Data.Word
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Maybe
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad
import Test.QuickCheck


data Midi = Midi {
    fileType :: FileType
  , timeDiv :: TimeDiv
  , tracks :: [Track Ticks]
  } deriving (Eq, Show)

instance Arbitrary Midi where
  arbitrary = do
    ft <- arbitrary
    td <- arbitrary
    if ft == SingleTrack
      then do
        trk <- arbitrary >>= return . fAux
        return $! Midi ft td [trk]
      else do
        trks <- arbitrary >>= return . map fAux
        return $! Midi ft td trks
    where
    fAux = (++ [(0,TrackEnd)]) . map (\(dt,m) -> (abs dt,m)) . removeTrackEnds 
  coarbitrary = undefined

data FileType = SingleTrack | MultiTrack | MultiPattern
  deriving (Eq, Show)

instance Arbitrary FileType where
  arbitrary = oneof [return SingleTrack , return MultiTrack , return MultiPattern]
  coarbitrary = undefined
  
type Track a = [(a,Message)]

data TimeDiv =
  TicksPerBeat Int | -- 1 -- (2^15 - 1)
  TicksPerSecond Int Int -- 1 - 127
             --  FramesPerSecond TicksPerFrame
  deriving (Show,Eq)

instance Arbitrary TimeDiv where
  arbitrary = oneof [
      choose (1,2 ^ (15 :: Int) - 1) >>= return . TicksPerBeat
    , two (choose (1,127)) >>= \(w1,w2) -> return $! TicksPerSecond w1 w2]
  coarbitrary = undefined

type Ticks = Int -- 0 - (2^28 - 1)
type Time = Double

type Channel = Int -- 0 - 15
type Key = Int -- 0 - 127
type Velocity = Int	-- 0 - 127
type Pressure = Int -- 0 - 127
type Preset = Int	-- 0 - 127
type Bank = Int
type PitchWheel = Int	-- 0 - (2^14 - 1)
type Tempo = Int -- microseconds per beat  1 - (2^24 - 1) 

data Message =
-- Channel Messages
  NoteOff         { channel :: !Channel, key :: !Key,              velocity :: !Velocity } |
  NoteOn          { channel :: !Channel, key :: !Key,              velocity :: !Velocity } |
  KeyPressure     { channel :: !Channel, key :: !Key,              pressure :: !Pressure} |
  ControlChange   { channel :: !Channel, controllerNumber :: !Int, controllerValue :: !Int } |
  ProgramChange   { channel :: !Channel, preset :: !Preset } |
  ChannelPressure { channel :: !Channel, pressure :: !Pressure } |
  PitchWheel      { channel :: !Channel, pitchWheel :: !PitchWheel } |
-- Meta Messages
  SequenceNumber !Int | -- 0 - (2^16 - 1) 
  Text !String |
  Copyright !String |
  TrackName !String |
  InstrumentName !String |
  Lyrics !String |
  Marker !String |
  CuePoint !String |
  ChannelPrefix !Channel |
  ProgramName !String |
  DeviceName !String |
  TrackEnd |
  TempoChange !Tempo |
  SMPTEOffset !Int !Int !Int !Int !Int | -- 0-23  0-59  0-59  0-30 0-99
  TimeSignature !Int !Int !Int !Int | -- 0-255  0-255   0-255   1-255
  KeySignature !Int !Int | -- -7 - 7  0 - 1
  Reserved !Int !L.ByteString |
  -- System Exclusive Messages
  Sysex !Int !L.ByteString -- 0xF0 or 0xF7
  deriving (Show,Eq)

instance Arbitrary Message where
  arbitrary = do
    -- Channel Messages
    c <- choose (0,15)
    oneof [
        two (choose (0,127))  >>= \(w2,w3) -> return $! NoteOff c w2 w3
      , two (choose (0,127))  >>= \(w2,w3) -> return $! NoteOn c w2 w3
      , two (choose (0,127))  >>= \(w2,w3) -> return $! KeyPressure c w2 w3
      , two (choose (0,127))  >>= \(w2,w3) -> return $! ControlChange c w2 w3
      , choose (0,127)        >>= \w2      -> return $! ProgramChange c w2
      , choose (0,127)        >>= \w2      -> return $! ChannelPressure c w2 
      , do p <- choose (0,2 ^ (14 :: Int) - 1)
           return $! PitchWheel c p
      -- Meta Messages
      , choose (0,2 ^ (16 :: Int) - 1) >>= return . SequenceNumber
      , arbitrary >>= return . Text
      , arbitrary >>= return . Copyright
      , arbitrary >>= return . TrackName
      , arbitrary >>= return . InstrumentName
      , arbitrary >>= return . Lyrics
      , arbitrary >>= return . Marker
      , arbitrary >>= return . CuePoint
      , return $! ChannelPrefix c
      , arbitrary >>= return . ProgramName
      , arbitrary >>= return . DeviceName
      , choose (0,2 ^ (14 :: Int) - 1) >>= return . TempoChange
      , do w1 <- choose (0,23)
           w2 <- choose (0,59)
           w3 <- choose (0,59)
           w4 <- choose (0,30)
           w5 <- choose (0,99)
           return $! SMPTEOffset w1 w2 w3 w4 w5
      , do w1 <- choose (0,255)
           w2 <- choose (0,255)
           w3 <- choose (0,255)
           w4 <- choose (1,255)
           return $! TimeSignature w1 w2 w3 w4
      , do w1 <- choose (-7,7)
           w2 <- choose (0,1)
           return $! KeySignature w1 w2
      , arbitrary >>= \bs -> return $! Reserved 0x60 bs
      -- System Exclusive Messages
      , do w <- oneof [return 0xF0, return 0xF7]
           bs <- arbitrary
           return $! Sysex w bs]
  coarbitrary = undefined

isNoteOff :: Message -> Bool
isNoteOff (NoteOff {}) = True
isNoteOff _ = False

isNoteOn :: Message -> Bool
isNoteOn (NoteOn {}) = True
isNoteOn _ = False

isKeyPressure :: Message -> Bool
isKeyPressure (KeyPressure {}) = True
isKeyPressure _ = False

isControlChange :: Message -> Bool
isControlChange (ControlChange {}) = True
isControlChange _ = False

isProgramChange :: Message -> Bool
isProgramChange (ProgramChange {}) = True
isProgramChange _ = False

isChannelPressure :: Message -> Bool
isChannelPressure (ChannelPressure {}) = True
isChannelPressure _ = False

isPitchWheel :: Message -> Bool
isPitchWheel (PitchWheel {}) = True
isPitchWheel _ = False

isChannelMessage :: Message -> Bool
isChannelMessage msg = (not $ isMetaMessage msg) && (not $ isSysexMessage msg)

isSysexMessage :: Message -> Bool
isSysexMessage (Sysex _ _) = True
isSysexMessage _ = False

isMetaMessage :: Message -> Bool
isMetaMessage msg = case msg of
  SequenceNumber _ -> True
  Text _ -> True
  Copyright _ -> True
  TrackName _ -> True
  InstrumentName _ -> True
  Lyrics _ -> True
  Marker _ -> True
  CuePoint _ -> True
  ChannelPrefix _ -> True
  ProgramName _ -> True
  DeviceName _ -> True
  TrackEnd -> True
  TempoChange _ -> True
  SMPTEOffset _ _ _ _ _ -> True
  TimeSignature _ _ _ _ -> True
  KeySignature _ _ -> True
  Reserved _ _ -> True
  _ -> False

isTrackEnd :: Message -> Bool
isTrackEnd TrackEnd = True
isTrackEnd _ = False

removeTrackEnds :: Track a -> Track a
removeTrackEnds [] = []
removeTrackEnds trk = filter (not. isTrackEnd . snd) trk

toSingleTrack :: Midi -> Midi
toSingleTrack m@(Midi SingleTrack _ _) = m
toSingleTrack (Midi MultiTrack td trks) = Midi SingleTrack td [trk']
  where trk' = foldl' merge [] trks
toSingleTrack (Midi MultiPattern td trks) = Midi SingleTrack td [trk']
  where trk' = (concat $ map removeTrackEnds trks) ++ [(0,TrackEnd)]

merge :: (Num a, Ord a) => Track a -> Track a -> Track a
merge track1 track2 = (fromAbsTime $ f trk1' trk2') ++ [(0,TrackEnd)]
  where
  trk1' = toAbsTime $ removeTrackEnds track1
  trk2' = toAbsTime $ removeTrackEnds track2
  f trk [] = trk
  f [] trk = trk
  f ((dt1,m1) : trk1) ((dt2,m2) : trk2) = if dt1 <= dt2
      then (dt1,m1) : (f trk1 ((dt2,m2) : trk2))
      else (dt2,m2) : (f ((dt1,m1) : trk1) trk2)

toAbsTime :: (Num a) => Track a -> Track a
toAbsTime trk = zip ts' ms
  where
  (ts,ms) = unzip trk
  (_,ts') = mapAccumL (\acc t -> let t' = acc + t in (t',t')) 0 ts 
  
fromAbsTime :: (Num a) => Track a -> Track a
fromAbsTime trk = zip ts' ms 
  where
  (ts,ms) = unzip trk
  (_,ts') = mapAccumL (\acc t -> (t,t - acc)) 0 ts 

toRealTime :: TimeDiv -> Track Ticks -> Track Time
toRealTime (TicksPerBeat tpb) trk = trk'
  where
  (_,trk') = mapAccumL f (div 60000000 120) trk -- default tempo 120 beats per minute
  formula dt tempo = 
    (fromIntegral dt / fromIntegral tpb) * (fromIntegral tempo) * (1.0E-6)
  f :: Tempo -> (Ticks,Message) -> (Tempo, (Time,Message))
  f _ (dt, TempoChange tempo) = (tempo, (formula dt tempo, TempoChange tempo))
  f tempo (dt,msg) = (tempo, (formula dt tempo,msg))
toRealTime (TicksPerSecond fps tpf) trk = map f trk
  where
  f (dt,msg) = (fromIntegral dt / (fromIntegral fps * fromIntegral tpf), msg)

fromRealTime :: TimeDiv -> Track Time -> Track Ticks
fromRealTime (TicksPerBeat tpb) trk = trk'
  where
  (_,trk') = mapAccumL f (div 60000000 120) trk -- default tempo 120 beats per minute
  formula dt tempo = round $ 
    (dt * fromIntegral tpb) / (fromIntegral tempo * 1.0E-6)
  f :: Tempo -> (Time,Message) -> (Tempo, (Ticks,Message))
  f _ (dt, TempoChange tempo) = (tempo, (formula dt tempo, TempoChange tempo))
  f tempo (dt,msg) = (tempo, (formula dt tempo,msg))
fromRealTime (TicksPerSecond fps tpf) trk = map f trk
  where
  f (dt,msg) = (round $ dt * fromIntegral fps * fromIntegral tpf, msg)

-- MIDI import 
importFile :: FilePath -> IO (Either String Midi)
importFile f = do
  bs <- L.readFile f
  return $! runParser parseMidi bs

exportFile :: FilePath -> Midi ->  IO ()
exportFile f m = do
  let bs = toLazyByteString $ buildMidi m
  L.writeFile f bs 

-- All numeric values are stored in big-endian format

parseMidi :: Parser Midi
parseMidi = do
  _ <- string "MThd"
  _ <- word32be 6
  formatType' <- getWord16be
  trackNumber' <- getWord16be
  timeDivision' <- getWord16be
  let timeDivision = if testBit timeDivision' 15
        then TicksPerSecond
               (fromIntegral $ (flip shiftR) 9  $ shiftL timeDivision' 1)
               (fromIntegral $ (flip shiftR) 8  $ shiftL timeDivision' 8)
        else TicksPerBeat (fromIntegral timeDivision')
  case (formatType',trackNumber') of
    (0,1) -> do
      track' <- parseTrack
      return $! Midi SingleTrack timeDivision [track']
    (1,n) -> do
      tracks' <- sequence $ replicate (fromIntegral n) parseTrack
      return $! Midi MultiTrack timeDivision tracks'
    (2,n) -> do
      tracks' <- sequence $ replicate (fromIntegral n) parseTrack
      return $! Midi MultiPattern timeDivision tracks'
    _ -> fail "Invalid Midi file format"

buildMidi :: Midi -> Builder
buildMidi m = mconcat [
    putString "MThd"
  , putWord32be 6
  , case fileType m of
      SingleTrack -> putWord16be 0 
      MultiTrack -> putWord16be 1
      MultiPattern -> putWord16be 2
  , putWord16be (fromIntegral $ length $ tracks m)
  , case timeDiv m of
      TicksPerBeat i -> putWord16be (fromIntegral i)
      TicksPerSecond i1 i2 -> mconcat [
          putWord8 (setBit (fromIntegral i1) 7)
        , putWord8 (fromIntegral i2)]
  , mconcat (map buildTrack $ tracks m)]
  
parseTrack :: Parser (Track Ticks)
parseTrack = do
  _ <- string "MTrk"
  _ <- getWord32be -- trackSize 
  track' <- parseMessages Nothing
  return track'
 
buildTrack :: Track Ticks -> Builder
buildTrack trk = mconcat [
    putString "MTrk"
  , putWord32be $ fromIntegral $ L.length bs
  , fromLazyByteString bs]
  where
  f (dt,msg) = (putVarLenBe $ fromIntegral dt) `append` buildMessage msg 
  bs = toLazyByteString $ mconcat (map f trk)

parseMessages :: Maybe Message -> Parser (Track Ticks)
parseMessages mPreMsg = do
  dt <- getVarLenBe >>= return . fromIntegral 
  msg <- parseMessage mPreMsg
  if (isTrackEnd msg)
    then return [(dt,msg)]
    else do
      let mMsg = if isChannelMessage msg then (Just msg) else mPreMsg
      msgs <- parseMessages mMsg
      return $! (dt,msg) : msgs

parseMessage :: Maybe Message -> Parser Message
parseMessage mPreMsg = choice [
      parseChannelMessage mPreMsg
    , parseMetaMessage
    , parseSysexMessage]
  
buildMessage :: Message -> Builder
buildMessage msg | isChannelMessage msg = buildChannelMessage msg
buildMessage msg | isMetaMessage msg = buildMetaMessage msg
buildMessage msg | isSysexMessage msg = buildSysexMessage msg
buildMessage _ = mempty
  
parseChannelMessage :: Maybe Message -> Parser Message
parseChannelMessage mPreMsg = choice $ map (\f -> f mPreMsg) [
      parseNoteOff
    , parseNoteOn
    , parseKeyPressure
    , parseControlChange
    , parseProgramChange
    , parseChannelPressure
    , parsePitchWheel
  ]

parseChannel :: Maybe Message -> (Message -> Bool) -> Word8 -> Parser Channel
parseChannel mPreMsg isNeededMsg msgCode = p1 <|> p2
  where
  p1 = do
    _ <- lookAhead (satisfy ( < 0x80))
    guard $ (isJust mPreMsg) && (isNeededMsg $ fromJust mPreMsg)
    return $! channel (fromJust mPreMsg)
  p2 = do
    w8 <- getWord8
    guard (msgCode == shiftR w8 4)
    return $! fromIntegral $ w8 .&. (0x0F :: Word8)

parseNoteOff :: Maybe Message -> Parser Message
parseNoteOff mPreMsg = do
  ch <- parseChannel mPreMsg isNoteOff 0x08
  p1 <- getWord8
  p2 <- getWord8
  return $! NoteOff ch (fromIntegral p1) (fromIntegral p2)

parseNoteOn :: Maybe Message -> Parser Message
parseNoteOn mPreMsg = do
  ch <- parseChannel mPreMsg isNoteOn 0x09
  p1 <- getWord8
  p2 <- getWord8
  return $! NoteOn ch (fromIntegral p1) (fromIntegral p2)

parseKeyPressure :: Maybe Message -> Parser Message
parseKeyPressure mPreMsg = do
  ch <- parseChannel mPreMsg isKeyPressure 0x0A
  p1 <- getWord8
  p2 <- getWord8
  return $! KeyPressure ch (fromIntegral p1) (fromIntegral p2)

parseControlChange :: Maybe Message -> Parser Message
parseControlChange mPreMsg = do
  ch <- parseChannel mPreMsg isControlChange 0x0B
  p1 <- getWord8
  p2 <- getWord8
  return $! ControlChange ch (fromIntegral p1) (fromIntegral p2)

parseProgramChange :: Maybe Message -> Parser Message
parseProgramChange mPreMsg = do
  ch <- parseChannel mPreMsg isProgramChange 0x0C
  p1 <- getWord8
  return $! ProgramChange ch (fromIntegral p1)

parseChannelPressure :: Maybe Message -> Parser Message
parseChannelPressure mPreMsg = do
  ch <- parseChannel mPreMsg isChannelPressure 0x0D
  p1 <- getWord8
  return $! ChannelPressure ch (fromIntegral p1)
  
parsePitchWheel :: Maybe Message -> Parser Message
parsePitchWheel mPreMsg = do
  ch <- parseChannel mPreMsg isPitchWheel 0x0E
  p1 <- getWord8
  p2 <- getWord8
  return $! PitchWheel ch $ (shiftL (fromIntegral p2) 7) .|. (fromIntegral p1)

buildChannelMessage :: Message -> Builder
buildChannelMessage msg = case msg of
  NoteOff         _ p1 p2  -> mconcat
    [f 0x08, putWord8 $ fromIntegral $ p1, putWord8 $ fromIntegral $ p2]
  NoteOn          _ p1 p2  -> mconcat
    [f 0x09, putWord8 $ fromIntegral $ p1, putWord8 $ fromIntegral $ p2]
  KeyPressure     _ p1 p2  -> mconcat
    [f 0x0A, putWord8 $ fromIntegral $ p1, putWord8 $ fromIntegral $ p2]
  ControlChange   _ p1 p2  -> mconcat
    [f 0x0B, putWord8 $ fromIntegral $ p1, putWord8 $ fromIntegral $ p2]
  ProgramChange   _ p1     -> mconcat [f 0x0C, putWord8 $ fromIntegral $ p1]
  ChannelPressure _ p1     -> mconcat [f 0x0D, putWord8 $ fromIntegral $ p1]
  PitchWheel      _ p1     -> mconcat [ f 0x0E
                                      , putWord8 (fromIntegral $ p1 .&. 0x7F)
                                      , putWord8 (fromIntegral $ shiftR p1 7)]
  _ -> mempty
  where
  f :: Int -> Builder
  f w8 = putWord8 $ fromIntegral $ (shiftL w8 4) .|. (channel msg)
  
parseMetaMessage :: Parser Message
parseMetaMessage = do
  _ <- word8 0xFF
  choice [
      parseSequenceNumber
    , parseText
    , parseCopyright
    , parseTrackName
    , parseInstrumentName
    , parseLyrics
    , parseMarker
    , parseCuePoint
    , parseChannelPrefix
    , parseProgramName
    , parseDeviceName
    , parseTrackEnd
    , parseTempoChange
    , parseSMPTEOffset
    , parseTimeSignature
    , parseKeySignature
    , parseReserved
    ]

buildMetaMessage :: Message -> Builder
buildMetaMessage msg = putWord8 0xFF `mappend`
  case msg of
    SequenceNumber i -> mconcat
      [putWord8 0x00, putVarLenBe 2, putWord16be $ fromIntegral $ i]
    Text s -> mconcat 
      [putWord8 0x01, putVarLenBe (fromIntegral $ length s), putString s]
    Copyright s -> mconcat
      [putWord8 0x02, putVarLenBe (fromIntegral $ length s), putString s]
    TrackName s -> mconcat
      [putWord8 0x03, putVarLenBe (fromIntegral $ length s), putString s]
    InstrumentName s -> mconcat
      [putWord8 0x04, putVarLenBe (fromIntegral $ length s), putString s]
    Lyrics s -> mconcat
      [putWord8 0x05, putVarLenBe (fromIntegral $ length s), putString s]
    Marker s -> mconcat
      [putWord8 0x06, putVarLenBe (fromIntegral $ length s), putString s]
    CuePoint s -> mconcat
      [putWord8 0x07, putVarLenBe (fromIntegral $ length s), putString s]
    ProgramName s -> mconcat
      [putWord8 0x08, putVarLenBe (fromIntegral $ length s), putString s]
    DeviceName s -> mconcat
      [putWord8 0x09, putVarLenBe (fromIntegral $ length s), putString s]
    ChannelPrefix i -> mconcat
      [putWord8 0x20, putVarLenBe 1, putWord8 $ fromIntegral $ i]
    TrackEnd -> putWord8 0x2F `mappend` putVarLenBe 0
    TempoChange i -> mconcat
      [putWord8 0x51, putVarLenBe 3, putWord24be $ fromIntegral $ i]
    SMPTEOffset i1 i2 i3 i4 i5 -> mconcat [
        putWord8 0x54
      , putVarLenBe 5
      , mconcat $ map (putWord8 . fromIntegral) [i1,i2,i3,i4,i5]]
    TimeSignature i1 i2 i3 i4 -> mconcat [
        putWord8 0x58
      , putVarLenBe 4
      , mconcat $ map (putWord8 . fromIntegral) [i1,i2,i3,i4]]
    KeySignature i1 i2 -> mconcat [
        putWord8 0x59
      , putVarLenBe 2
      , putInt8  $ fromIntegral $ i1
      , putWord8 $ fromIntegral $ i2]
    Reserved w bs -> mconcat [
        putWord8 (fromIntegral w)
      , putVarLenBe (fromIntegral $ L.length bs)
      , fromLazyByteString bs]
    _ -> mempty  

parseSequenceNumber :: Parser Message
parseSequenceNumber = do
  _ <- word8 0x00
  _ <- varLenBe 2
  n <- getWord16be
  return $! SequenceNumber (fromIntegral n)

parseText :: Parser Message
parseText = do
  _ <- word8 0x01
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! Text s

parseCopyright :: Parser Message
parseCopyright = do
  _ <- word8 0x02
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! Copyright s

parseTrackName :: Parser Message
parseTrackName = do
  _ <- word8 0x03
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! TrackName s

parseInstrumentName :: Parser Message
parseInstrumentName = do
  _ <- word8 0x04
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! InstrumentName s

parseLyrics :: Parser Message
parseLyrics = do
  _ <- word8 0x05
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! Lyrics s

parseMarker :: Parser Message
parseMarker = do
  _ <- word8 0x06
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! Marker s

parseCuePoint :: Parser Message
parseCuePoint = do
  _ <- word8 0x07
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! CuePoint s

parseProgramName :: Parser Message
parseProgramName = do
  _ <- word8 0x08
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! ProgramName s

parseDeviceName :: Parser Message
parseDeviceName = do
  _ <- word8 0x09
  l <- getVarLenBe
  s <- getString (fromIntegral l)
  return $! DeviceName s

parseChannelPrefix :: Parser Message
parseChannelPrefix = do
  _ <- word8 0x20
  _ <- varLenBe 1
  p <- getWord8
  return $! ChannelPrefix (fromIntegral p)

parseTrackEnd :: Parser Message
parseTrackEnd =  do
  _ <- word8 0x2F
  _ <- varLenBe 0
  return $! TrackEnd

parseTempoChange :: Parser Message
parseTempoChange = do
  _ <- word8 0x51
  _ <- varLenBe 3
  t <- getWord24be
  return $! TempoChange (fromIntegral t)

parseSMPTEOffset :: Parser Message
parseSMPTEOffset = do
  _ <- word8 0x54
  _ <- varLenBe 5
  bs <- getLazyByteString 5 
  let [n1,n2,n3,n4,n5] = map fromIntegral (L.unpack bs)
  return $! SMPTEOffset n1 n2 n3 n4 n5

parseTimeSignature :: Parser Message
parseTimeSignature = do
  _ <- word8 0x58
  _ <- varLenBe 4
  bs <- getLazyByteString 4
  let [n1,n2,n3,n4] = map fromIntegral (L.unpack bs)
  return $! TimeSignature n1 n2 n3 n4

parseKeySignature :: Parser Message
parseKeySignature = do
  _ <- word8 0x59
  _ <- varLenBe 2
  n1 <- getInt8
  n2 <- getWord8
  return $! KeySignature (fromIntegral n1) (fromIntegral n2)

parseReserved :: Parser Message
parseReserved = do
  t <- getWord8
  l <- getVarLenBe
  bs <- getLazyByteString (fromIntegral l)
  return $! Reserved (fromIntegral t)  bs

parseSysexMessage :: Parser Message
parseSysexMessage = do
  w <- (word8 0xF0) <|> (word8 0xF7)
  l <- getVarLenBe
  d <- getLazyByteString (fromIntegral l)
  return $! Sysex (fromIntegral w) d

buildSysexMessage :: Message -> Builder
buildSysexMessage (Sysex i bs) =
  mconcat [ putWord8 $ fromIntegral $ i
          , putVarLenBe $ fromIntegral $ L.length bs
          , fromLazyByteString bs]
buildSysexMessage _ = mempty
