module Billboard.Tests (mainTestFile, mainTestDir) where

import Test.HUnit
import Control.Monad (void)
import Data.List (genericLength)

import HarmTrace.Audio.ChordTypes (TimedData, onset, offset, Timed)

import Billboard.BillboardParser (parseBillboard)
import Billboard.BillboardData (BillboardData (..), BBChord (..))

-- constants
acceptableBeatDeviationMultiplier :: Double
acceptableBeatDeviationMultiplier = 0.5

-- applyTestToList :: (String -> Test) -> [String] -> Test
-- applyTestToList t fps = TestList (map t fps)

-- oneChordTimedList :: String -> Test
-- oneChordTimedList = 
  -- TestCase . assertBool "More then one chord in chord list"
           -- . and . map oneChord . getSong . fst . parseBillboard 
    -- where oneChord :: TimedData BBChord -> Bool
          -- oneChord = allSame . getData

oddBeatLength :: BillboardData -> Test
oddBeatLength bbd = 
  let song   = getSong bbd
      avgLen = (sum $ map beatDuration song) / (genericLength song)
      minLen = avgLen *  acceptableBeatDeviationMultiplier
      maxLen = avgLen * (acceptableBeatDeviationMultiplier + 1)
  in TestCase (assertBool ("odd Beat length detected for " ++ getTitle bbd)
              (and . map (rangeCheck minLen maxLen) $ song))
                   
isInRange :: Double -> Double -> TimedData BBChord -> Test
isInRange minLen maxLen t = 
  TestCase (assertBool  ("odd Beat length detected for " ++ show t) 
                        (rangeCheck minLen maxLen t))
           
rangeCheck :: Double -> Double -> TimedData a -> Bool
rangeCheck minLen maxLen t = let len = beatDuration t 
                             in  len >= minLen && len <= maxLen

beatDuration :: Timed t => t a -> Double
beatDuration t = offset t - onset t
          
-- oneChordTimedListVerbose :: String -> Test
-- oneChordTimedListVerbose = 
  -- TestList . map oneChord . getSong . fst . parseBillboard 
    -- where oneChord :: TimedData BBChord -> Test
          -- oneChord c = TestCase $ assertBool 
            -- (show c ++ " contains more then one chord in chord list" )
            -- (allSame $ getData c)
            
-- allSame :: [BBChord] -> Bool  
-- allSame (c:cs) = null $ dropWhile ((== chord c) . chord) cs

-- testing one File
mainTestFile :: FilePath -> IO ()
mainTestFile fp = return ()

-- testing a directory of files
mainTestDir :: FilePath -> IO ()
mainTestDir fp = readFile fp >>= void . runTestTT . oddBeatLength 
                                                  . fst . parseBillboard
              -- do cs <- mapM readFile fps 
                 -- void . runTestTT $ applyTestToList oneChordTimedList cs
  -- f fp = readFile fp >>= void . runTest . oneChordTimedListVerbose
