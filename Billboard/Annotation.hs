{-# OPTIONS_GHC -Wall #-}

module Billboard.Annotation ( Annotation (..), Label (..), isStruct
                , isStart, Instrument (..), Description (..)
                ,  isUnknown, getLabel, isRepeat, getRepeats) where

import HarmTrace.Base.MusicRep (Root)
-- import Data.List (sortBy, groupBy)
-- import Data.Function  (on)

data Annotation = Start Label | End Label  deriving Eq

instance Show Annotation where
  show (Start l) = '<' : show l  
  show (End   l) = show l ++ ">"  
  -- show StartEnd = "><" 

data Label = Struct     Char Int    -- denoting A .. Z and a nr of primes (')
           | Instr      Instrument
           | Anno       Description
           | Modulation Root
       deriving Eq

instance Show Label where
  show (Instr      lab) = show lab
  show (Anno       lab) = show lab
  show (Modulation lab) = show lab
  show (Struct     c i) = c : replicate i '\''

data Instrument = Guitar | Voice | Violin   | Banjo | Synthesizer | Saxophone
                | Flute  | Drums | Trumpet  | Piano | Harmonica   | Organ 
                | Keyboard       | Trombone | Electricsitar   | Pennywhistle 
                | Tenorsaxophone | Whistle  | Oboe  | Tambura | Horns | Clarinet
                | Electricguitar | Tenorhorn | Percussion | Rhythmguitar 
                | Hammondorgan   | Harpsichord | Cello    | Acousticguitar
                | Bassguitar     | Strings   | SteelDrum  | Vibraphone | Bongos
                | Steelguitar    | UnknownInstr String
       deriving (Show, Eq)
       
data Description = Chorus  | Intro | Outro | Bridge  | Interlude | Solo
                 | Fadeout | Fadein | Prechorus | Maintheme   | Keychange 
                 | Secondarytheme   | Ending    | PhraseTrans | Instrumental 
                 | Coda    | Transistion | PreVerse   |Vocal  | Talking
                 | Repeat Int
                 | Verse  (Maybe Int)
                 | UnknownAnno String
       deriving (Show, Eq)
       
 
-- showIntGTone :: Int -> String
-- showIntGTone i = if i > 1 then show i else ""

-- addBr :: String -> Int -> String
-- addBr s i = if i > 1 then "\n" ++ s else ""

--------------------------------------------------------------------------------
-- Boundary Utilities
--------------------------------------------------------------------------------

getLabel :: Annotation -> Label
getLabel (Start l) = l
getLabel (End   l) = l

isStart :: Annotation -> Bool
isStart (Start _) = True
isStart (End   _) = False

isStruct :: Label -> Bool
isStruct (Struct _ _) = True
isStruct _            = False

isUnknown :: Annotation -> Bool
isUnknown s = case (getLabel s) of 
  (Instr (UnknownInstr _ )) -> True
  (Anno  (UnknownAnno  _ )) -> True
  _                         -> False

-- | Returns True if the 'Annotation' represents a repeat.
isRepeat :: Annotation -> Bool
isRepeat (End (Anno (Repeat _))) = True
isRepeat _                       = False

-- | Returns the number of repeats represented by this 'Annotation'. If the
-- 'Annotation' describes something completely different (say 'Electricsitar')
-- it will return 1.
getRepeats :: Annotation -> Int
getRepeats (End (Anno (Repeat r))) = r
getRepeats _                       = 1