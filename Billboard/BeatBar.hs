--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.BillboardParser
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <W.B.deHaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Modelling musical time (in a minimalistic way) with beats, bars
-- and time signatures.
--------------------------------------------------------------------------------

module Billboard.BeatBar (TimeSig (..), BeatWeight (..), beatWeight) where

--------------------------------------------------------------------------------
-- Modelling Beat and Bar structures (for chord sequences)
--------------------------------------------------------------------------------

-- TODO explain Change: perhaps this should be separated
-- | Barlines can have different weights. Among other applications, this is used
-- in the printing of chord sequences.
data BeatWeight = UnAligned | Beat | Change | Bar | Bar4 | Bar8 
                | Bar16 | LineStart
       deriving (Eq, Ord, Enum) 

instance Show BeatWeight where
  show UnAligned = "" 
  show Change    = "" 
  show Beat      = "." 
  show Bar       = "|" 
  show Bar4      = "|\n|" 
  show Bar8      = "|\n|" 
  show Bar16     = "|\n|" 
  show LineStart = "|\n|" 

-- | Model a time signature as a fraction
newtype TimeSig = TimeSig {timeSig :: (Int, Int)} deriving (Eq)

instance Show TimeSig where
  show (TimeSig (num, denom)) = show num ++ '/' : show denom

-- | Defines the "metrical weight of a bar". A regular beat has strength 0, 
-- a bar has strength 1, a bar after 4 bars 2, a bar after 8 bars 3, and a bar 
-- after 16 bars 4.
beatWeight :: TimeSig -> Int -> BeatWeight
beatWeight ts pos = let beat = getNrBeats ts in
  case (mod pos beat, mod pos (4*beat), mod pos (8*beat), mod pos (16*beat)) of
    (0,0,0,0) -> Bar16 -- a bar @ sixteen measures, weight 4
    (0,0,0,_) -> Bar8  -- a bar @ eight measures
    (0,0,_,_) -> Bar4  -- a bar @ four measures
    (0,_,_,_) -> Bar   -- a bar position
    _         -> Beat  -- a regular beat position, weight 0 
    
-- | Returns the number of beats in a bar
getNrBeats :: TimeSig -> Int
getNrBeats    (TimeSig (beats,  4)) = beats
getNrBeats ts@(TimeSig (eights, 8)) 
  | even eights = div eights 2
  | otherwise   = irregularMeterError ts "cannot round the number of beats"
getNrBeats ts   = irregularMeterError ts "cannot round the number of beats"

irregularMeterError :: TimeSig -> String -> a
irregularMeterError ts msg = error ("Irregular meter: " ++ show ts ++ ' ' : msg)


