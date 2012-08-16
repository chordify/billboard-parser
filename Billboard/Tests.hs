{-# OPTIONS_GHC -Wall      #-}
{-# LANGUAGE TupleSections #-}
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

import HarmTrace.Audio.ChordTypes (TimedData, onset, getData, Timed)

import Billboard.BillboardParser (parseBillboard)
import Billboard.BillboardData (BillboardData (..), BBChord (..))
import Billboard.IOUtils
import Billboard.PostProcessing (rangeCheck, getMinMaxBeatLen, beatDuration)
 
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
mainTestDir fp = getBBFiles fp >>= mapM readParse >>=
                 void . runTestTT . applyTestToList oddBeatLengthTest where

  readParse :: (FilePath, Int) -> IO (BillboardData, Int)
  readParse (f,i) = readFile f >>= return . (,i) . fst . parseBillboard

--------------------------------------------------------------------------------
-- The unit tests
--------------------------------------------------------------------------------

-- | Tests the all beat lengths in a song and reports per song. 
oddBeatLengthTest :: (BillboardData, Int) -> Test
oddBeatLengthTest (bbd, bbid) = 
  let song                = getSong bbd
      (_, minLen, maxLen) = getMinMaxBeatLen song
  in TestCase (assertBool ("odd Beat length detected for:\n" 
                          ++ show bbid ++ ": " ++ getTitle bbd)
              (and . map (rangeCheck minLen maxLen) $ song))

-- | Creates a test out of 'rangeCheck': this test reports on every chord 
-- whether or not the beat length is within the the allowed range of 
-- beat length deviation, as set by 'acceptableBeatDeviationMultiplier'.
rangeTest :: Double -> Double -> TimedData BBChord -> Test
rangeTest minLen maxLen td = 
  TestCase (assertBool  ("Odd Beat length detected for:\n" ++ showChord td) 
                        (rangeCheck minLen maxLen td)) where
    
    -- | give some infromation about what is wrong
    showChord :: TimedData BBChord -> String
    showChord t =  (show . chord . getData $ t) ++ ", length: " 
                ++ (show . beatDuration $ t) ++ " @ " ++ (show . onset $ t)

--------------------------------------------------------------------------------
-- Some testing related utitlities
--------------------------------------------------------------------------------

-- | Applies a test to a list of testable items
applyTestToList :: (a -> Test) -> [a] -> Test
applyTestToList testf a = TestList (map testf a)


