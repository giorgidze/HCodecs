-----------------------------------------------------------------------------
-- |
-- Module      : Codec.SoundFont
-- Copyright   : George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : Experimental
-- Portability : Portable
--
-- Module for reading and writting of SoundFont instrument description files.
--
-----------------------------------------------------------------------------

module Codec.SoundFont (
    SoundFont (..)
  , Info (..)
  , Sdta (..)
  , Pdta (..)
  , Phdr (..)
  , Bag (..)
  , Mod (..)
  , Generator (..)
  , isSampleIndex
  , isInstIndex
  , Inst (..)
  , Shdr (..)
  , importFile
  , exportFile
  , parseSoundFont
  , buildSoundFont
  , parseInfos
  , buildInfos
  , parseSdta
  , buildSdta
  , parsePdta
  , buildPdta
  )  where

import Data.ByteString.Parser
import Data.ByteString.Builder
import Data.Arbitrary
import qualified Data.Audio as Audio

import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (w2c)
import Data.Array.IArray
import Data.List

import Test.QuickCheck
import Data.Monoid
import Control.Applicative
import Control.Monad

data SoundFont = SoundFont {
    infos :: Array Word Info -- Suplemental information
  , sdta  :: Sdta -- The Sample Binary Data
  , pdta :: Pdta 
  -- articulation :: Articulation -- The Preset, Instrument, and Sample Header data
  } deriving (Eq, Show)

instance Arbitrary SoundFont where
  arbitrary = do
    f1 <- arbitrary; f2 <- arbitrary; f3 <- arbitrary;
    return $! SoundFont f1 f2 f3

-- instance Show SoundFont where
--   show sf = (show $ length $ elems $ sampleData sf) ++ "\n" ++ (show $ length $ show $ articulation sf) ++ "\n"

-- type SamplePointIndex = Word32
-- type SampleData = UArray SamplePointIndex Audio.Sample

data Info = 
    Version Word Word -- Refers to file version of SounfFont RIFF file
  | TargetSoundEngine String -- Refers to target Sound Engine
  | BankName String -- Refers to SoundFont Bank Name
  | RomName String -- Refers to Sound ROM Name
  | RomVersion Word Word -- Refers to Sound ROM Version
  | CreationDate String -- Refers to Date of Creation of the Bank
  | Authors String -- Sound Designers and Engineers of the Bank
  | IntendedProduct String -- Product fot which the bank was intended
  | CopyrightMessage String -- Contains any copyright message
  | Comments String -- Contains any comemnts on the Bank
  | UsedTools String -- The SoundFont tools used to create and alter the bank
  | ReservedInfo String Word L.ByteString
  deriving (Eq,Show)

instance Arbitrary Info where
  arbitrary = oneof [
      do (w1,w2) <- two $ choose (minBound :: Word16, maxBound);
         return $ Version (fromIntegral w1) (fromIntegral w2);
    , do l <- choose (0,255); s <- genStringNul l; return  $ TargetSoundEngine s
    , do l <- choose (0,255); s <- genStringNul l; return  $ BankName s
    , do l <- choose (0,255); s <- genStringNul l; return  $ RomName s
    , do (w1,w2) <- two $ choose (minBound :: Word16, maxBound);
         return $ RomVersion (fromIntegral w1) (fromIntegral w2);
    , do l <- choose (0,255); s <- genStringNul l; return  $ CreationDate s
    , do l <- choose (0,255); s <- genStringNul l; return  $ Authors s
    , do l <- choose (0,255); s <- genStringNul l; return  $ IntendedProduct s
    , do l <- choose (0,255); s <- genStringNul l; return  $ CopyrightMessage s    
    , do l <- choose (0,255); s <- genStringNul l; return  $ Comments s
    , do l <- choose (0,255); s <- genStringNul l; return  $ UsedTools s
    , do l <- choose (0,255); s <- vector (fromIntegral l);
         return  $ ReservedInfo "RSRV" l (L.pack s)]
    where
    genStringNul :: Int -> Gen String 
    genStringNul l = sequence $ replicate l $ fmap w2c $ choose (1,255)

data Sdta = Sdta {
    smpl :: Audio.SampleData Int16
  , sm24 :: Maybe (Audio.SampleData Int8)
  } deriving (Eq, Show)

instance Arbitrary Sdta where
  arbitrary = do
    sn <- choose (1,1024)
    smpl1 <- arrayGen sn
    oneof [
        return $! Sdta smpl1 Nothing
      , do sm24' <- arrayGen sn
           return $! Sdta smpl1 (Just sm24')
      ]

data Pdta = Pdta {
    phdrs :: Array Word Phdr
  , pbags :: Array Word Bag
  , pmods :: Array Word Mod
  , pgens :: Array Word Generator
  , insts :: Array Word Inst
  , ibags :: Array Word Bag
  , imods :: Array Word Mod
  , igens :: Array Word Generator
  , shdrs :: Array Word Shdr
  } deriving (Eq, Show)
  
instance Arbitrary Pdta where
  arbitrary = do
    f1 <- arbitrary; f2 <- arbitrary; f3 <- arbitrary; f4 <- arbitrary;
    f5 <- arbitrary; f6 <- arbitrary; f7 <- arbitrary; f8 <- arbitrary;
    f9 <- arbitrary;
    return $! Pdta f1 f2 f3 f4 f5 f6 f7 f8 f9

data Phdr = Phdr {
    presetName :: String
  , preset :: Word
  , bank :: Word
  , presetBagNdx :: Word
  , library :: Word
  , genre :: Word
  , morphology :: Word
  } deriving (Eq, Show)

instance Arbitrary Phdr where
  arbitrary = do
    n <- choose (0,20)
    presetName' <- stringNulGen n
    preset' <- choose (minBound :: Word16, maxBound)
    bank' <- choose (minBound :: Word16, maxBound)
    presetBagNdx' <- choose (minBound :: Word16, maxBound)
    library' <- choose (minBound :: Word32, maxBound)
    genre' <- choose (minBound :: Word32, maxBound)
    morphology' <- choose (minBound :: Word32, maxBound)
    return $ Phdr {
        presetName = presetName'
      , preset = fromIntegral $ preset'
      , bank = fromIntegral $ bank'
      , presetBagNdx = fromIntegral $ presetBagNdx'
      , library = fromIntegral $ library'
      , genre = fromIntegral $ genre'
      , morphology = fromIntegral $ morphology'
      }

data Bag = Bag {
    genNdx :: Word
  , modNdx :: Word
  } deriving (Eq, Show)
  
instance Arbitrary Bag where
  arbitrary = do
    genNdx' <- choose (minBound :: Word16, maxBound)
    modNdx' <- choose (minBound :: Word16, maxBound)    
    return $! Bag {
        genNdx = fromIntegral genNdx'
      , modNdx = fromIntegral modNdx'}

data Mod = Mod {
    srcOper :: Word
  , destOper :: Word
  , amount :: Int
  , amtSrcOper :: Word
  , transOper :: Word
  } deriving (Eq, Show)
  
instance Arbitrary Mod where
  arbitrary = do
    srcOper' <- choose (minBound :: Word16, maxBound)
    destOper' <- choose (minBound :: Word16, maxBound)    
    amount' <- choose (minBound :: Int16, maxBound)
    amtSrcOper' <- choose (minBound :: Word16, maxBound)    
    transOper' <- choose (minBound :: Word16, maxBound)                
    return $! Mod {
        srcOper = fromIntegral srcOper'
      , destOper = fromIntegral destOper'
      , amount = fromIntegral amount'
      , amtSrcOper = fromIntegral amtSrcOper'
      , transOper = fromIntegral transOper'
      }

data Generator =
  -- Oscillator
  StartAddressOffset Int | -- 0 sample start fine offset
  EndAddressOffset Int | -- 1 sample end fine offset
  LoopStartAddressOffset Int | -- 2 sample start loop fine offset
  LoopEndAddressOffset Int |  -- 3 sample end loop file offset
  StartAddressCoarseOffset Int | -- 4 sample start coarse offset
  ModLfoToPitch Int | -- 5 main fm: modLfo-> pitch
  VibLfoToPitch Int | -- 6 aux fm:  vibLfo-> pitch
  ModEnvToPitch Int | -- 7 pitch env: modEnv(aux)-> pitch

  -- Filter
  InitFc Int | -- 8 initial filter cutoff
  InitQ Int | -- 9 filter Q
  ModLfoToFc Int | -- 10 filter modulation: lfo1 -> filter cutoff
  ModEnvToFc Int | -- 11 filter env: modEnv(aux)-> filter cutoff

  
  EndAddressCoarseOffset Int | -- 12 initial volume
  
  -- Amplifier
  ModLfoToVol Int | -- 13 tremolo: lfo1-> volume
  -- 14 unused

  -- Effects
  Chorus Int | -- 15 chorus
  Reverb Int | -- 16 reverb
  Pan Int | -- 17 pan
  -- 18 unused
  -- 19 unused
  -- 20 unused

  -- Modulation LFO
  DelayModLfo Int | -- 21 delay 
  FreqModLfo Int | -- 22 frequency

  -- Vibrato LFO
  DelayVibLfo Int | -- 23 delay 
  FreqVibLfo Int | -- 24 frequency

  -- Modulation Envelope
  DelayModEnv Int | -- 25 delay 
  AttackModEnv Int | -- 26 attack
  HoldModEnv Int | -- 27 hold
  DecayModEnv Int | -- 28 decay
  SustainModEnv Int | -- 29 sustain
  ReleaseModEnv Int | -- 30 release
  KeyToModEnvHold Int | -- 31 key scaling coefficient
  KeyToModEnvDecay Int | -- 32 key scaling coefficient

  -- Volume Envelope (ampl/vol)
  DelayVolEnv Int | -- 33 delay 
  AttackVolEnv Int | -- 34 attack
  HoldVolEnv Int | -- 35 hold
  DecayVolEnv Int | -- 36 decay
  SustainVolEnv Int | -- 37 sustain
  ReleaseVolEnv Int | -- 38 release
  KeyToVolEnvHold Int | -- 39 key scaling coefficient
  KeyToVolEnvDecay Int | -- 40 key scaling coefficient

  -- Preset
  InstIndex Word | -- 41
  -- 42
  KeyRange Word Word | -- 43
  VelRange Word Word | -- 44
  LoopStartAddressCoarseOffset Int | -- 45 
  Key Word | -- 46
  Vel Word | -- 47
  InitAtten Int | -- 48
  -- 49 unused
  LoopEndAddressCoarseOffset Int | -- 50

  CoarseTune Int | -- 51
  FineTune Int | -- 52
  SampleIndex Word | -- 53
  SampleMode Audio.SampleMode | -- 54
  --  55 unused
  ScaleTuning Int | -- 56
  ExclusiveClass Int | -- 57
       
  RootKey Word | -- 58
  -- 59 unused
  ReservedGen Int Int -- 60 single constructor for all unused generators
  deriving (Eq, Show)

instance Arbitrary Generator where
  arbitrary = do
    i <- choose (minBound :: Int16, maxBound)  >>= return . fromIntegral
    w <- choose (minBound :: Word16, maxBound)  >>= return . fromIntegral
    i' <- choose (60 :: Int16, maxBound)  >>= return . fromIntegral
    r1 <- choose (0,127)
    r2 <- choose (0,127)
    smplMode' <- arbitrary
    oneof $ map return [
        StartAddressOffset i
      , EndAddressOffset i 
      , LoopStartAddressOffset i
      , LoopEndAddressOffset i
      , StartAddressCoarseOffset i
      , ModLfoToPitch i
      , VibLfoToPitch i
      , ModEnvToPitch i
      
      , InitFc i
      , InitQ i
      , ModLfoToFc i
      , ModEnvToFc i
    
      , EndAddressCoarseOffset i
      , ModLfoToVol i
    
      , Chorus i
      , Reverb i
      , Pan i
      , DelayModLfo i
      , FreqModLfo i
    
      , DelayVibLfo i
      , FreqVibLfo i
    
      , DelayModEnv i
      , AttackModEnv i
      , HoldModEnv i
      , DecayModEnv i
      , SustainModEnv i
      , ReleaseModEnv i
      , KeyToModEnvHold i
      , KeyToModEnvDecay i
    
      , DelayVolEnv i
      , AttackVolEnv i
      , HoldVolEnv i
      , DecayVolEnv i
      , SustainVolEnv i
      , ReleaseVolEnv i
      , KeyToVolEnvHold i
      , KeyToVolEnvDecay i
    
      , InstIndex w
    
      , KeyRange r1 r2
      , VelRange r2 r2
      , LoopStartAddressCoarseOffset i
      , Key w
      , Vel w
      , InitAtten i
    
      , LoopEndAddressCoarseOffset i
    
      , CoarseTune i
      , FineTune i
      , SampleIndex w
      , SampleMode smplMode'
    
      , ScaleTuning i
      , ExclusiveClass i
         
      , RootKey w
    
      , ReservedGen i' i]

isSampleIndex :: Generator -> Bool
isSampleIndex g = case g of
  SampleIndex _ -> True
  _ -> False 

isInstIndex :: Generator -> Bool
isInstIndex g = case g of
  InstIndex _ -> True
  _ -> False

data Inst = Inst {
    instName :: String
  , instBagNdx :: Word
  } deriving (Eq, Show)

instance Arbitrary Inst where
  arbitrary = do
    n <- choose (0,20)
    instName' <- stringNulGen n
    instBagNdx' <- choose (maxBound :: Word16, minBound)
    return $! Inst {
        instName = instName'
      , instBagNdx = fromIntegral $ instBagNdx'}

data Shdr = Shdr {
    sampleName :: String
  , start :: Word
  , end :: Word
  , startLoop :: Word
  , endLoop :: Word
  , sampleRate :: Word
  , originalPitch :: Word
  , pitchCorrection :: Int
  , sampleLink :: Word
  , sampleType :: Word
  } deriving (Eq, Show)

instance Arbitrary Shdr where
  arbitrary = do
    n <- choose (0,20)
    sampleName' <- stringNulGen n
    start' <- choose (minBound :: Word32, maxBound)
    end' <- choose (minBound :: Word32, maxBound)
    startLoop' <- choose (minBound :: Word32, maxBound)
    endLoop' <- choose (minBound :: Word32, maxBound)
    sampleRate' <- choose (minBound :: Word32, maxBound)
    originalPitch' <- choose (minBound :: Word8, maxBound)
    pitchCorrection' <- choose (minBound :: Int8, maxBound)
    sampleLink' <- choose (minBound :: Word16, maxBound)
    sampleType' <- choose (minBound :: Word16, maxBound)    
    return $ Shdr {
        sampleName = sampleName'
      , start = fromIntegral start'
      , end = fromIntegral end'
      , startLoop = fromIntegral startLoop'
      , endLoop = fromIntegral endLoop'
      , sampleRate = fromIntegral sampleRate'
      , originalPitch = fromIntegral originalPitch'
      , pitchCorrection = fromIntegral pitchCorrection'
      , sampleLink = fromIntegral sampleLink'
      , sampleType = fromIntegral sampleType'
      }

---- SoundFont import

importFile :: FilePath -> IO (Either String SoundFont)
importFile n = do
  bs <- L.readFile n
  return $! runParser parseSoundFont bs

exportFile :: FilePath -> SoundFont ->  IO ()
exportFile f sf = do
  let bs = toLazyByteString $ buildSoundFont sf
  L.writeFile f bs 

parseSoundFont :: Parser SoundFont
parseSoundFont = do
  _ <- string "RIFF"
  _ <- getWord32le -- chunkSize
  _ <- string "sfbk"
  infos' <- parseInfos
  sdta' <- parseSdta
  pdta' <- parsePdta
  return $! SoundFont {
      infos = infos'
    , sdta = sdta'
    , pdta = pdta'
    }

buildSoundFont :: SoundFont -> Builder
buildSoundFont sf = mconcat [
    putString "RIFF"
  , putWord32le $ fromIntegral chunkSize
  , fromLazyByteString bs]
  where
  chunkSize = L.length bs
  bs = toLazyByteString $ mconcat [
      putString "sfbk"
    , buildInfos (infos sf)
    , buildSdta (sdta sf)
    , buildPdta (pdta sf)]

--buildSoundFont :: SoundFont -> Builder
--buildSoundFont sf = mconcat [

parseInfos :: Parser (Array Word Info)
parseInfos = do
  _ <- string "LIST"
  _ <- getWord32le -- chunkSize
  _ <- string "INFO"
  infos' <- many p
  return $! listArray (0, genericLength infos' - 1) infos'
  where
  p = choice [
      do n <- getString 4; _ <- word32le 4; w1 <- getWord16le; w2 <- getWord16le;
         case n of
           "ifil" -> return $! Version (fromIntegral w1) (fromIntegral w2)
           "iver" -> return $! RomVersion (fromIntegral w1) (fromIntegral w2)
           _ -> fail []
    , do n <- getString 4; l <- expect (<= 256) getWord32le; s <- getStringNul;  
         skip (fromIntegral l - genericLength s - 1);
         case n of
           "isng" -> return $! TargetSoundEngine s
           "INAM" -> return $! BankName s
           "irom" -> return $! RomName s
           "ICRD" -> return $! CreationDate s
           "IENG" -> return $! Authors s
           "IPRD" -> return $! IntendedProduct s
           "ICOP" -> return $! CopyrightMessage s
           "ICMT" -> return $! Comments s
           "ISFT" -> return $! UsedTools s
           _ -> fail []
    , do n <- expect ( /= "LIST") (getString 4)
         l <- getWord32le 
         bs <- getLazyByteString (fromIntegral l)
         return $! ReservedInfo n (fromIntegral l) bs]

buildInfos :: (Array Word Info) -> Builder
buildInfos infos' = mconcat [
    putString "LIST"
  , putWord32le $ (fromIntegral $ L.length bs) + 4
  , putString "INFO"
  , fromLazyByteString bs]
  where
  bs = toLazyByteString $ mconcat $ map buildInfo $ elems infos'

{-
Specification says that 'ifil' 'isgn' and 'INAM' fields are mandatory
but I am not checking it during parsing
-}


buildInfo :: Info -> Builder
buildInfo (Version w1 w2) = mconcat
  [putString "ifil", putWord32le 4,
   putWord16le (fromIntegral w1), putWord16le (fromIntegral w2)]
buildInfo (RomVersion w1 w2) = mconcat
  [putString "iver", putWord32le 4,
   putWord16le (fromIntegral w1), putWord16le (fromIntegral w2)]
buildInfo (TargetSoundEngine s) = mconcat [putString "isng", buildInfoString s]
buildInfo (BankName s) = mconcat [putString "INAM", buildInfoString s]
buildInfo (RomName s) = mconcat [putString "irom", buildInfoString s]
buildInfo (CreationDate s) = mconcat [putString "ICRD", buildInfoString s]
buildInfo (Authors s) = mconcat [putString "IENG", buildInfoString s]
buildInfo (IntendedProduct s) = mconcat [putString "IPRD", buildInfoString s]
buildInfo (CopyrightMessage s) = mconcat [putString "ICOP", buildInfoString s]
buildInfo (Comments s) = mconcat [putString "ICMT", buildInfoString s]
buildInfo (UsedTools s) = mconcat [putString "ISFT", buildInfoString s]
buildInfo (ReservedInfo n l bs) = mconcat
  [putString n, putWord32le (fromIntegral l), fromLazyByteString bs]
  
buildInfoString :: String -> Builder
buildInfoString s = if (mod l 2 == 0)
  then mconcat [putWord32le (l + 2), putString s, putWord8 0, putWord8 0]
  else mconcat [putWord32le (l + 1), putString s, putWord8 0]
  where
  l = fromIntegral $ length s
  
parseSdta :: Parser Sdta
parseSdta = do
  _ <- string "LIST"
  sdtaSize <- getWord32le >>= return .fromIntegral
  _ <- string "sdta"
  _ <- string "smpl"
  smplSize <- getWord32le  >>= return .fromIntegral
  when (odd smplSize) $ fail "'smplSize' must not be odd number"
  let sn = div smplSize 2
  smpl' <- Audio.parseSampleData sn getInt16le
  choice [
      do guard (smplSize == (sdtaSize - 12))
         return $! Sdta {smpl = smpl', sm24 =  Nothing}
    , do _ <- string "sm24"
         let sm24Size = if odd sn then sn + 1 else sn 
         _ <- word32le (fromIntegral sm24Size)
         sm24' <- Audio.parseSampleData sn getInt8
         skip (fromIntegral $ sm24Size - sn)
         return $! Sdta{ smpl = smpl', sm24 = Just sm24'}
    ]

buildSdta :: Sdta -> Builder
buildSdta (Sdta smpl1 Nothing) = mconcat [
    putString "LIST"
  , putWord32le $ fromIntegral $ sdtaSize
  , putString "sdta"
  , putString "smpl"
  , putWord32le $ fromIntegral $ smplSize
  , Audio.buildSampleData putInt16le smpl1]
  where smplSize = (Audio.sampleNumber smpl1) * 2
        sdtaSize = 4 + 4 + 4 + smplSize
buildSdta (Sdta smpl1 (Just sd8)) = mconcat [
    putString "LIST"
  , putWord32le $ fromIntegral $ sdtaSize
  , putString "sdta"
  , putString "smpl"
  , putWord32le $ fromIntegral $ smplSize
  , Audio.buildSampleData putInt16le smpl1
  , putString "sm24"
  , putWord32le $ fromIntegral $ sm24Size
  , Audio.buildSampleData putInt8 sd8
  , mconcat $ genericReplicate (sm24Size - sn) $ putWord8 0]
  where sn = Audio.sampleNumber smpl1
        smplSize = sn * 2
        sm24Size = if odd sn then sn + 1 else sn
        sdtaSize = 4 + 4 + 4 + smplSize + 4 + 4 + sm24Size

parsePdta :: Parser Pdta
parsePdta = do
  _ <- string "LIST"
  _ <- getWord32le -- pdtaSize
  _ <- string "pdta"
  phdrs' <- parseSubchunk "phdr" 38 parsePhdr
  pbags' <- parseSubchunk "pbag" 4 parseBag
  pmods' <- parseSubchunk "pmod" 10 parseMod
  pgens' <- parseSubchunk "pgen" 4  parseGen
  insts' <- parseSubchunk "inst" 22  parseInst  
  ibags' <- parseSubchunk "ibag" 4 parseBag
  imods' <- parseSubchunk "imod" 10 parseMod
  igens' <- parseSubchunk "igen" 4  parseGen
  shdrs' <- parseSubchunk "shdr" 46 parseShdr
  return $! Pdta phdrs' pbags' pmods' pgens' insts' ibags' imods' igens' shdrs'

buildPdta :: Pdta -> Builder
buildPdta pdta1 = mconcat [
    putString "LIST"
  , putWord32le $ fromIntegral chunkSize
  , fromLazyByteString bs]
  where
  chunkSize = L.length bs
  bs = toLazyByteString $ mconcat [
      putString "pdta"
    , buildSubchunk "phdr" 38 buildPhdr (phdrs pdta1)
    , buildSubchunk "pbag"  4 buildBag  (pbags pdta1)
    , buildSubchunk "pmod" 10 buildMod  (pmods pdta1)
    , buildSubchunk "pgen"  4 buildGen  (pgens pdta1)
    , buildSubchunk "inst" 22 buildInst (insts pdta1)
    , buildSubchunk "ibag"  4 buildBag  (ibags pdta1)
    , buildSubchunk "imod" 10 buildMod  (imods pdta1)
    , buildSubchunk "igen"  4 buildGen  (igens pdta1)    
    , buildSubchunk "shdr" 46 buildShdr (shdrs pdta1)    
    ]

-- For some subchunks minimal number of records is two
-- but this check can be done later I am skiping it here
parseSubchunk :: String -> Word -> (Parser a) -> Parser (Array Word a)
parseSubchunk s size p = do
  _ <- string s
  chunkSize <- expect (\w -> mod w size == 0) (getWord32le >>= return . fromIntegral)
  let n = div chunkSize size
  cs <- sequence (genericReplicate n p)
  return $! listArray (0, n - 1) cs

buildSubchunk :: String -> Word -> (a -> Builder) -> (Array Word a) ->  Builder
buildSubchunk s size b a = mconcat [
    putString s
  , putWord32le $ fromIntegral $ (1 + (snd $ bounds a)) * size
  , mconcat $ map b $ elems a]
  
parsePhdr :: Parser Phdr  
parsePhdr = do
  presetName' <- getLazyByteString 20
                 >>= return . map w2c . L.unpack . L.takeWhile ( /= 0)
  preset' <- getWord16le >>= return . fromIntegral
  bank' <- getWord16le >>= return . fromIntegral
  presetBagNdx' <- getWord16le >>= return . fromIntegral
  library' <- getWord32le >>= return . fromIntegral
  genre' <- getWord32le >>= return . fromIntegral
  morphology' <- getWord32le >>= return . fromIntegral
  return $ Phdr {
      presetName = presetName'
    , preset = preset'
    , bank = bank'
    , presetBagNdx = presetBagNdx'
    , library = library'
    , genre = genre'
    , morphology = morphology'
    }

buildPhdr :: Phdr -> Builder  
buildPhdr phdr = mconcat [
      putString $ presetName phdr
    , mconcat $ replicate (20 - length (presetName phdr)) (putWord8 0) 
    , putWord16le $ fromIntegral $ preset phdr
    , putWord16le $ fromIntegral $ bank phdr
    , putWord16le $ fromIntegral $ presetBagNdx phdr
    , putWord32le $ fromIntegral $ library phdr
    , putWord32le $ fromIntegral $ genre phdr
    , putWord32le $ fromIntegral $ morphology phdr    
    ]

parseBag :: Parser Bag
parseBag = do
    genNdx' <- getWord16le
    modNdx' <- getWord16le
    return $! Bag {
        genNdx = fromIntegral genNdx'
      , modNdx = fromIntegral modNdx'}
    
buildBag :: Bag -> Builder  
buildBag bag = mconcat [
      putWord16le $ fromIntegral $ genNdx bag
    , putWord16le $ fromIntegral $ modNdx bag]

parseMod :: Parser Mod
parseMod = do
    srcOper' <- getWord16le
    destOper' <- getWord16le
    amount' <- getInt16le
    amtSrcOper' <- getWord16le
    transOper' <- getWord16le
    return $! Mod {
        srcOper = fromIntegral srcOper'
      , destOper = fromIntegral destOper'
      , amount = fromIntegral amount'
      , amtSrcOper = fromIntegral amtSrcOper'
      , transOper = fromIntegral transOper'
      }
    
buildMod :: Mod -> Builder
buildMod m = mconcat [
      putWord16le $ fromIntegral $ srcOper m
    , putWord16le $ fromIntegral $ destOper m
    , putWord16le $ fromIntegral $ amount m
    , putWord16le $ fromIntegral $ amtSrcOper m
    , putWord16le $ fromIntegral $ transOper m
    ]

parseGen :: Parser Generator
parseGen = choice [
    int16le  0 >> getInt16le >>= return . StartAddressOffset . fromIntegral
  , int16le  1 >> getInt16le >>= return . EndAddressOffset . fromIntegral
  , int16le  2 >> getInt16le >>= return . LoopStartAddressOffset . fromIntegral
  , int16le  3 >> getInt16le >>= return . LoopEndAddressOffset . fromIntegral
  , int16le  4 >> getInt16le  >>= return . StartAddressCoarseOffset . fromIntegral
  , int16le  5 >> getInt16le  >>= return . ModLfoToPitch . fromIntegral
  , int16le  6 >> getInt16le  >>= return . VibLfoToPitch . fromIntegral
  , int16le  7 >> getInt16le  >>= return . ModEnvToPitch . fromIntegral

  , int16le  8 >> getInt16le  >>= return . InitFc . fromIntegral
  , int16le  9 >> getInt16le  >>= return . InitQ . fromIntegral
  , int16le 10 >> getInt16le  >>= return . ModLfoToFc . fromIntegral
  , int16le 11 >> getInt16le  >>= return . ModEnvToFc . fromIntegral
    
  , int16le 12 >> getInt16le  >>= return . EndAddressCoarseOffset . fromIntegral
  , int16le 13 >> getInt16le  >>= return . ModLfoToVol . fromIntegral
    
  , int16le 15 >> getInt16le  >>= return . Chorus . fromIntegral
  , int16le 16 >> getInt16le  >>= return . Reverb . fromIntegral
  , int16le 17 >> getInt16le  >>= return . Pan . fromIntegral
    
  , int16le 21 >> getInt16le  >>= return . DelayModLfo . fromIntegral
  , int16le 22 >> getInt16le  >>= return . FreqModLfo . fromIntegral
    
  , int16le 23 >> getInt16le  >>= return . DelayVibLfo . fromIntegral
  , int16le 24 >> getInt16le  >>= return . FreqVibLfo . fromIntegral
    
  , int16le 25 >> getInt16le  >>= return . DelayModEnv . fromIntegral
  , int16le 26 >> getInt16le  >>= return . AttackModEnv . fromIntegral
  , int16le 27 >> getInt16le  >>= return . HoldModEnv . fromIntegral
  , int16le 28 >> getInt16le  >>= return . DecayModEnv . fromIntegral
  , int16le 29 >> getInt16le  >>= return . SustainModEnv . fromIntegral
  , int16le 30 >> getInt16le  >>= return . ReleaseModEnv . fromIntegral
  , int16le 31 >> getInt16le  >>= return . KeyToModEnvHold . fromIntegral
  , int16le 32 >> getInt16le  >>= return . KeyToModEnvDecay . fromIntegral
    
  , int16le 33 >> getInt16le  >>= return . DelayVolEnv . fromIntegral
  , int16le 34 >> getInt16le  >>= return . AttackVolEnv . fromIntegral
  , int16le 35 >> getInt16le  >>= return . HoldVolEnv . fromIntegral
  , int16le 36 >> getInt16le  >>= return . DecayVolEnv . fromIntegral
  , int16le 37 >> getInt16le  >>= return . SustainVolEnv . fromIntegral
  , int16le 38 >> getInt16le  >>= return . ReleaseVolEnv . fromIntegral
  , int16le 39 >> getInt16le  >>= return . KeyToVolEnvHold . fromIntegral
  , int16le 40 >> getInt16le  >>= return . KeyToVolEnvDecay . fromIntegral
    
  , int16le 41 >> getWord16le >>= return . InstIndex . fromIntegral
  , do _ <- int16le 43; a <- getWord8 >>= return . fromIntegral;
       b <- getWord8 >>= return . fromIntegral; return $ KeyRange a b;
  , do _ <- int16le 44; a <- getWord8 >>= return . fromIntegral;
       b <- getWord8 >>= return . fromIntegral; return $ VelRange a b;
    
  , int16le 45 >> getInt16le  >>= return . LoopStartAddressCoarseOffset . fromIntegral
  , int16le 46 >> getWord16le  >>= return . Key . fromIntegral
  , int16le 47 >> getWord16le  >>= return . Vel . fromIntegral
  , int16le 48 >> getInt16le  >>= return . InitAtten . fromIntegral
  , int16le 50 >> getInt16le  >>= return . LoopEndAddressCoarseOffset . fromIntegral
    
  , int16le 51 >> getInt16le  >>= return . CoarseTune . fromIntegral
  , int16le 52 >> getInt16le  >>= return . FineTune . fromIntegral
  , int16le 53 >> getWord16le >>= return . SampleIndex . fromIntegral
  , do _ <- int16le  54;  a <- getInt16le;
       case a of
         1 -> return $ SampleMode Audio.ContLoop
         3 -> return $ SampleMode Audio.PressLoop
         _ -> return $ SampleMode Audio.NoLoop
  , int16le 56 >> getInt16le  >>= return . ScaleTuning . fromIntegral
  , int16le 57 >> getInt16le  >>= return . ExclusiveClass . fromIntegral
    
  , int16le 58 >> getWord16le  >>= return . RootKey . fromIntegral
  , do p1 <- getInt16le; p2 <- getInt16le;
       return $ ReservedGen (fromIntegral p1) (fromIntegral p2)]


buildGen :: Generator -> Builder
buildGen g = mconcat $ case g of
  StartAddressOffset i       -> [putInt16le 0, putInt16le $ fromIntegral i]
  EndAddressOffset i         -> [putInt16le 1, putInt16le $ fromIntegral i]
  LoopStartAddressOffset i   -> [putInt16le 2, putInt16le $ fromIntegral i]
  LoopEndAddressOffset i     -> [putInt16le 3, putInt16le $ fromIntegral i]
  StartAddressCoarseOffset i -> [putInt16le 4, putInt16le $ fromIntegral i]
  ModLfoToPitch i            -> [putInt16le 5, putInt16le $ fromIntegral i]
  VibLfoToPitch i            -> [putInt16le 6, putInt16le $ fromIntegral i]
  ModEnvToPitch i            -> [putInt16le 7, putInt16le $ fromIntegral i]

  InitFc i     -> [putInt16le 8, putInt16le $ fromIntegral i]
  InitQ i      -> [putInt16le 9, putInt16le $ fromIntegral i]
  ModLfoToFc i -> [putInt16le 10, putInt16le $ fromIntegral i]
  ModEnvToFc i -> [putInt16le 11, putInt16le $ fromIntegral i]
    
  EndAddressCoarseOffset i     -> [putInt16le 12, putInt16le $ fromIntegral i]
  ModLfoToVol i -> [putInt16le 13, putInt16le $ fromIntegral i]
   
  Chorus i -> [putInt16le 15, putInt16le $ fromIntegral i]
  Reverb i -> [putInt16le 16, putInt16le $ fromIntegral i]
  Pan i    -> [putInt16le 17, putInt16le $ fromIntegral i]
  
  DelayModLfo i -> [putInt16le 21, putInt16le $ fromIntegral i]
  FreqModLfo  i -> [putInt16le 22, putInt16le $ fromIntegral i]
    
  DelayVibLfo i -> [putInt16le 23, putInt16le $ fromIntegral i]
  FreqVibLfo i  -> [putInt16le 24, putInt16le $ fromIntegral i]
    
  DelayModEnv i      -> [putInt16le 25, putInt16le $ fromIntegral i]
  AttackModEnv i     -> [putInt16le 26, putInt16le $ fromIntegral i]
  HoldModEnv i       -> [putInt16le 27, putInt16le $ fromIntegral i]
  DecayModEnv i      -> [putInt16le 28, putInt16le $ fromIntegral i]
  SustainModEnv i    -> [putInt16le 29, putInt16le $ fromIntegral i]
  ReleaseModEnv i    -> [putInt16le 30, putInt16le $ fromIntegral i]
  KeyToModEnvHold i  -> [putInt16le 31, putInt16le $ fromIntegral i]
  KeyToModEnvDecay i -> [putInt16le 32, putInt16le $ fromIntegral i]
    
  DelayVolEnv i      -> [putInt16le 33, putInt16le $ fromIntegral i]
  AttackVolEnv i     -> [putInt16le 34, putInt16le $ fromIntegral i]
  HoldVolEnv i       -> [putInt16le 35, putInt16le $ fromIntegral i]
  DecayVolEnv i      -> [putInt16le 36, putInt16le $ fromIntegral i]
  SustainVolEnv i    -> [putInt16le 37, putInt16le $ fromIntegral i]
  ReleaseVolEnv i    -> [putInt16le 38, putInt16le $ fromIntegral i]
  KeyToVolEnvHold i  -> [putInt16le 39, putInt16le $ fromIntegral i]
  KeyToVolEnvDecay i -> [putInt16le 40, putInt16le $ fromIntegral i]
    
  InstIndex i  -> [putInt16le 41, putWord16le $ fromIntegral i]
  KeyRange a b -> [putInt16le 43, putWord8 $ fromIntegral a, putWord8 $ fromIntegral b]
  VelRange a b -> [putInt16le 44, putWord8 $ fromIntegral a, putWord8 $ fromIntegral b]

  LoopStartAddressCoarseOffset i  -> [putInt16le 45, putInt16le $ fromIntegral i]
  Key i                           -> [putWord16le 46, putInt16le $ fromIntegral i]
  Vel i                           -> [putWord16le 47, putInt16le $ fromIntegral i]
  InitAtten i                     -> [putInt16le 48, putInt16le $ fromIntegral i]
  LoopEndAddressCoarseOffset i    -> [putInt16le 50, putInt16le $ fromIntegral i]
    
  CoarseTune i  -> [putInt16le 51, putInt16le $ fromIntegral i]
  FineTune i    -> [putInt16le 52, putInt16le $ fromIntegral i]
  SampleIndex i -> [putInt16le 53, putWord16le $ fromIntegral i]
  
  SampleMode Audio.ContLoop  -> [putInt16le 54, putInt16le 1]
  SampleMode Audio.PressLoop -> [putInt16le 54, putInt16le 3]
  SampleMode Audio.NoLoop    -> [putInt16le 54, putInt16le 2]
  --56
  ScaleTuning i -> [putInt16le 56, putInt16le $ fromIntegral i]
  ExclusiveClass i -> [putInt16le 57, putInt16le $ fromIntegral i]
    
  RootKey i -> [putInt16le 58, putWord16le $ fromIntegral i]
  ReservedGen i1 i2 ->  [putInt16le $ fromIntegral i1, putInt16le $ fromIntegral i2]

parseInst :: Parser Inst  
parseInst = do
  instName' <- getLazyByteString 20
               >>= return . map w2c . L.unpack . L.takeWhile ( /= 0)
  instBagNdx' <- getWord16le >>= return . fromIntegral
  return $ Inst {
      instName = instName'
    , instBagNdx = instBagNdx'}

buildInst :: Inst -> Builder  
buildInst i = mconcat [
      putString $ instName i
    , mconcat $ replicate (20 - length (instName i)) (putWord8 0) 
    , putWord16le $ fromIntegral $ instBagNdx i]

parseShdr :: Parser Shdr  
parseShdr = do
  sampleName' <- getLazyByteString 20
                 >>= return . map w2c . L.unpack . L.takeWhile ( /= 0)
  start' <- getWord32le
  end' <- getWord32le
  startLoop' <- getWord32le
  endLoop' <- getWord32le  
  sampleRate' <- getWord32le
  originalPitch' <- getWord8
  pitchCorrection' <- getInt8
  sampleLink' <- getWord16le
  sampleType' <- getWord16le
  return $ Shdr {
      sampleName = sampleName'
    , start = fromIntegral start'
    , end = fromIntegral end'
    , startLoop = fromIntegral startLoop'
    , endLoop = fromIntegral endLoop'
    , sampleRate = fromIntegral sampleRate'
    , originalPitch = fromIntegral originalPitch'
    , pitchCorrection = fromIntegral pitchCorrection'
    , sampleLink = fromIntegral sampleLink'
    , sampleType = fromIntegral sampleType'
    }

buildShdr :: Shdr -> Builder  
buildShdr shdr = mconcat [
      putString $ sampleName shdr
    , mconcat $ replicate (20 - length (sampleName shdr)) (putWord8 0) 
    , putWord32le $ fromIntegral $ start shdr
    , putWord32le $ fromIntegral $ end shdr    
    , putWord32le $ fromIntegral $ startLoop shdr
    , putWord32le $ fromIntegral $ endLoop shdr
    , putWord32le $ fromIntegral $ sampleRate shdr
    , putWord8    $ fromIntegral $ originalPitch shdr
    , putInt8     $ fromIntegral $ pitchCorrection shdr    
    , putWord16le $ fromIntegral $ sampleLink shdr
    , putWord16le $ fromIntegral $ sampleType shdr
    ]