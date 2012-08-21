module Billboard.PostProcessing (
                                  postProcess
                                -- , acceptableBeatDeviationMultiplier
                                , rangeCheck
                                , getMinMaxBeatLen
                                , beatDuration
                                ) where

import Data.List (genericLength)
                                
import HarmTrace.Base.MusicRep hiding (isNone)
import HarmTrace.Audio.ChordTypes (TimedData, Timed (..))
import HarmTrace.Tokenizer.Tokenizer (pRoot, pChord)

import Billboard.BillboardData 

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

acceptableBeatDeviationMultiplier :: Double
acceptableBeatDeviationMultiplier = 0.10

--------------------------------------------------------------------------------
-- Top level function
-------------------------------------------------------------------------------

postProcess :: [TimedData BBChord] -> [TimedData BBChord]
postProcess dat = dat


--------------------------------------------------------------------------------
-- Some beat length estimation utilities
--------------------------------------------------------------------------------

-- | Returns True if the 'beatDuration' of a 'TimedData' item lies between 
-- the minimum (first argument) and the maximum (second argument) value
rangeCheck :: Double -> Double -> TimedData BBChord -> Bool
rangeCheck minLen maxLen t = let len = beatDuration t 
                             in  (len >= minLen && len <= maxLen) ||
                                 -- None chords are not expected to be 
                                 -- beat aligned and are ignored
                                 (isNoneBBChord . getData $ t)

-- | Given a 'TimedData', returns a triplet containing the average beat length,
-- the minimum beat length and the maximum beat length, respectively.
getMinMaxBeatLen :: [TimedData BBChord] -> (Double, Double, Double)
getMinMaxBeatLen song =
  let avgLen = (sum $ map beatDuration song) / (genericLength song)
  in ( avgLen                                            -- average beat length
     , avgLen *  acceptableBeatDeviationMultiplier       -- minimum beat length
     , avgLen * (acceptableBeatDeviationMultiplier + 1)) -- maximum beat length

-- | Calculates the duration of a beat
beatDuration :: Timed t => t a -> Double
beatDuration t = offset t - onset t
