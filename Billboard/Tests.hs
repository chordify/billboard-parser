{-# OPTIONS_GHC -Wall      #-}
{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.Tests
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of unit tests for testing the billboard-parser
--------------------------------------------------------------------------------

module Billboard.Tests ( mainTestFile
                       , mainTestDir
                       , oddBeatLengthTest
                       , reduceTest
                       , reduceTestVerb
                       , rangeTest
                       , getOffBeats) where

import Test.HUnit
import Control.Monad             ( void )

import Data.List                 ( genericLength )

import HarmTrace.Base.Time       ( Timed, onset, offset, getData )

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData   ( BillboardData (..), BBChord (..)
                                 , isNoneBBChord
                                 , reduceTimedBBChords, expandTimedBBChords
                                 ) -- , getBBChords )
import Billboard.IOUtils

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

testBeatDeviationMultiplier :: Float
testBeatDeviationMultiplier = 0.075

--------------------------------------------------------------------------------
-- Top level testing functions
--------------------------------------------------------------------------------

-- | Testing one File
mainTestFile :: (BillboardData -> IO Test) -> FilePath -> IO ()
mainTestFile testf fp =
  do readFile fp >>= return . fst . parseBillboard
                 >>= testf
                 >>= void . runTestTT

-- | testing a directory of files
mainTestDir :: ((BillboardData, Int) -> Test )-> FilePath -> IO ()
mainTestDir t fp = getBBFiles fp >>= mapM readParse >>=
                 void . runTestTT . applyTestToList t where

  readParse :: (FilePath, Int) -> IO (BillboardData, Int)
  readParse (f,i) = readFile f >>= return . (,i) . fst . parseBillboard

--------------------------------------------------------------------------------
-- The unit tests
--------------------------------------------------------------------------------

-- | Tests the all beat lengths in a song and reports per song.
oddBeatLengthTest :: (BillboardData, Int) -> Test
oddBeatLengthTest (bbd, bbid) =
  let song = filter (not . isNoneBBChord . getData ) . getSong $ bbd
      (_, minLen, maxLen) = getMinMaxBeatLen song
  in TestCase (assertBool ("odd Beat length detected for:\n"
                          ++ show bbid ++ ": " ++ getTitle bbd)
              (and . map (rangeCheck minLen maxLen) $ song))



-- | Creates a test out of 'rangeCheck': this test reports on every chord
-- whether or not the beat length is within the the allowed range of
-- beat length deviation, as set by 'testBeatDeviationMultiplier'.
rangeTest :: BillboardData -> IO Test
rangeTest d = do let (avgL, minL, maxL) = getMinMaxBeatLen . getSong $ d
                 putStrLn ("average beat length: " ++ show avgL)
                 return . applyTestToList (testf minL maxL) . getSong $ d where

    testf :: Float -> Float -> Timed BBChord -> Test
    testf minLen maxLen t =
      TestCase (assertBool  ("Odd Beat length detected for:\n" ++ showChord t)
                            (rangeCheck minLen maxLen t))

showChord :: Timed BBChord -> String
showChord t =  (show . chord . getData $ t) ++ ", length: "
            ++ (show . beatDuration $ t) ++ " @ " ++ (show . onset $ t)

-- | Returns True if the 'beatDuration' of a 'Timed' item lies between
-- the minimum (first argument) and the maximum (second argument) value
rangeCheck :: Float -> Float -> Timed BBChord -> Bool
rangeCheck minLen maxLen t = let len = beatDuration t
                             in  (len >= minLen && len <= maxLen) ||
                                 -- None chords are not expected to be
                                 -- beat aligned and are ignored
                                 (isNoneBBChord . getData $ t)

-- | Given a 'Timed', returns a triplet containing the average beat length,
-- the minimum beat length and the maximum beat length, respectively.
getMinMaxBeatLen :: [Timed BBChord] -> (Float, Float, Float)
getMinMaxBeatLen = getMinMaxBeatLen' testBeatDeviationMultiplier

getMinMaxBeatLen' :: Float -> [Timed BBChord] -> (Float, Float, Float)
getMinMaxBeatLen' mult song =
  let chds   = filter (not . isNoneBBChord . getData ) song
      avgLen = (sum $ map beatDuration chds) / (genericLength chds)
  in ( avgLen                                            -- average beat length
     , avgLen *  mult       -- minimum beat length
     , avgLen * (mult + 1)) -- maximum beat length

-- | Tests whether: ('expandBBChords' . 'reduceBBChords' $ cs) == cs
reduceTest :: (BillboardData, Int) -> Test
reduceTest (bbd, i) =
  let cs = getSong bbd
  in TestCase
       (assertBool ("reduce mismatch for id " ++ show i)
       (and $ zipWith (\a b -> getData a == getData b)
                      (expandTimedBBChords . reduceTimedBBChords $ cs) cs))

-- | Tests whether: ('expandBBChords' . 'reduceBBChords' $ cs) == cs
reduceTestVerb :: BillboardData -> IO Test
reduceTestVerb bbd =
  do let cs = getSong bbd
     return . applyTestToList cTest $
              zip (expandTimedBBChords . reduceTimedBBChords $ cs) cs where

  cTest :: (Timed BBChord, Timed BBChord) -> Test
  cTest (a,b) = TestCase
    (assertBool ("non-maching chords: " ++ show a ++ " and " ++ show b)
    (getData a == getData b)) -- N.B. we derive Eq

getOffBeats :: Float -> [Timed BBChord] -> [Timed BBChord]
getOffBeats th song = let (_avg, mn, mx) = getMinMaxBeatLen' th song
                      in  filter (not . rangeCheck mn mx) song


--------------------------------------------------------------------------------
-- Some testing related utitlities
--------------------------------------------------------------------------------

-- | Calculates the duration of a beat
beatDuration :: Timed a -> Float
beatDuration t = offset t - onset t

-- | Applies a test to a list of testable items
applyTestToList :: (a -> Test) -> [a] -> Test
applyTestToList testf a = TestList (map testf a)
