{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.Tests
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <W.B.deHaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of unit tests for testing the billboard-parser
--------------------------------------------------------------------------------

module Billboard.Tests ( mainTestFile, mainTestDir ) where

import Test.HUnit
import Control.Monad (void)
import Data.List (genericLength)

import HarmTrace.Audio.ChordTypes (TimedData, onset, offset, getData, Timed)

import Billboard.BillboardParser (parseBillboard)
import Billboard.BillboardData (BillboardData (..), BBChord (..))
import Billboard.IOUtils

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

acceptableBeatDeviationMultiplier :: Double
acceptableBeatDeviationMultiplier = 0.5
 
--------------------------------------------------------------------------------
-- Top level testing functions
--------------------------------------------------------------------------------

-- | Testing one File
mainTestFile :: FilePath -> IO ()
mainTestFile fp = 
  do song <- readFile fp >>= return . getSong . fst . parseBillboard
     let (avgLen, minLen, maxLen) = getMinMaxBeatLen song
     putStrLn ("average beat length: " ++ show avgLen)
     void . runTestTT . applyTestToList (rangeTest minLen maxLen) $ song

-- | testing a directory of files
mainTestDir :: FilePath -> IO ()
mainTestDir = void . bbdir testFile where

  testFile :: FilePath -> IO (Counts)
  testFile f = readFile f >>= runTestTT . oddBeatLengthTest 
                                        . fst . parseBillboard

--------------------------------------------------------------------------------
-- The unit tests
--------------------------------------------------------------------------------

-- | Tests the all beat lengths in a song and reports per song. 
oddBeatLengthTest :: BillboardData -> Test
oddBeatLengthTest bbd = 
  let song                = getSong bbd
      (_, minLen, maxLen) = getMinMaxBeatLen song
  in TestCase (assertBool ("odd Beat length detected for " ++ getTitle bbd)
              (and . map (rangeCheck minLen maxLen) $ song))

-- | Creates a test out of 'rangeCheck': this test reports on every chord 
-- whether or not the beat length is within the the allowed range of 
-- beat length deviation, as set by 'acceptableBeatDeviationMultiplier'.
rangeTest :: Double -> Double -> TimedData BBChord -> Test
rangeTest minLen maxLen t = 
  TestCase (assertBool  ("Odd Beat length detected for: " ++ showChord t) 
                        (rangeCheck minLen maxLen t))

showChord :: TimedData BBChord -> String
showChord t =  (show . chord . getData $ t) ++ ", length: " 
            ++ (show . beatDuration $ t)

-- | Returns True if the 'beatDuration' of a 'TimedData' item lies between 
-- the minimum (first argument) and the maximum (second argument) value
rangeCheck :: Double -> Double -> TimedData a -> Bool
rangeCheck minLen maxLen t = let len = beatDuration t 
                             in  len >= minLen && len <= maxLen

-- | Given a 'TimedData', returns a triplet containing the average beat length,
-- the minimum beat length and the maximum beat length, respectively.
getMinMaxBeatLen :: [TimedData BBChord] -> (Double, Double, Double)
getMinMaxBeatLen song =
  let avgLen = (sum $ map beatDuration song) / (genericLength song)
  in ( avgLen                                            -- average beat length
     , avgLen *  acceptableBeatDeviationMultiplier       -- minimum beat length
     , avgLen * (acceptableBeatDeviationMultiplier + 1)) -- maximum beat length
     
--------------------------------------------------------------------------------
-- Some testing related utitlities
--------------------------------------------------------------------------------

-- | Calculates the duration of a beat
beatDuration :: Timed t => t a -> Double
beatDuration t = offset t - onset t

-- | Applies a test to a list of testable items
applyTestToList :: (a -> Test) -> [a] -> Test
applyTestToList testf a = TestList (map testf a)


