HCodecs - A library to read, write and manipulate MIDI, WAVE, and SoundFont2 files.

Code example
===

This programs generates a short WAV file "sample.wav", then reads it and prints each sample.

```
import Codec.Wav ( exportFile, importFile )
import Data.Audio ( Audio(Audio) )
import Data.Array.Unboxed ( listArray, elems )
import Data.Int ( Int32 )
import Data.Maybe ( fromMaybe )
import System.IO (FilePath)

filename = "sample.wav"

sinewave :: [Float]
sinewave = map (\x -> sin ((fromInteger x) / 10.0)) [1..10000]

outMain :: FilePath -> IO ()
outMain path = do
  let fs :: [Float]
      fs = sinewave
      l = 1000
      rate = 44100
  -- Float is 32bit, so i use Int32 for each sample in the output.
  let maxInt32 :: Float
      maxInt32 = fromIntegral (maxBound::Int32)
      -- actual transformation function
      rounder :: Float -> Int32
      rounder = round . (*maxInt32)
  exportFile path ( Audio rate 1 -- 1 channel
                  $ listArray (0,l)
                  $ map rounder
                  $ fs)
                  
inMain :: FilePath -> IO ()
inMain path = do
  maybeAudio <- importFile path
  case maybeAudio :: Either String (Audio Int32) of
    Left s -> putStrLn $ "wav decoding error: " ++ s
    Right (Audio rate channels samples) -> do
      putStrLn $ "rate = " ++ show rate
      putStrLn $ "channels: " ++ show channels
      print $ elems samples

main = do
  putStrLn $ "* Outputting the sound to "++filename
  outMain filename
  putStrLn $ "* Printing the content of "++filename
  inMain filename

-- Original by hexagoxel @ freenode/#haskell
-- Modified by Vitaly "_Vi" Shukela
```
