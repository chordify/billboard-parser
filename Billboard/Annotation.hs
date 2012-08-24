{-# OPTIONS_GHC -Wall #-}
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
-- Summary: Billboard files can contain a variety of annoatations. This module
-- aims at representing these as Haskell types
--------------------------------------------------------------------------------

module Billboard.Annotation ( Annotation (..), Label (..), isStruct
                , isStart, Instrument (..), Description (..)
                , isUnknown, getLabel, isRepeat, getRepeats
                , isBeginOrEndAnno, isEndAnno) where

import HarmTrace.Base.MusicRep (Root)

-- | an 'Annotation' occurs either at the start or at the end of a chord 
-- sequence line.
data Annotation = Start Label | End Label  deriving Eq

instance Show Annotation where
  show (Start l) = '<' : show l  
  show (End   l) = show l ++ ">"  

-- | All annotations contain information we term 'Label'
data Label = Struct     Char Int   -- ^ denoting A .. Z and the nr of primes (')
           | Instr      Instrument 
           | Anno       Description
           | Modulation Root 
       deriving Eq

instance Show Label where
  show (Instr      lab) = show lab
  show (Anno       lab) = show lab
  show (Modulation lab) = show lab
  show (Struct     c i) = c : replicate i '\''

-- | Representing musical instruments
data Instrument = Guitar | Voice | Violin   | Banjo | Synthesizer | Saxophone
                | Flute  | Drums | Trumpet  | Piano | Harmonica   | Organ 
                | Keyboard       | Trombone | Electricsitar   | Pennywhistle 
                | Tenorsaxophone | Whistle  | Oboe  | Tambura | Horns | Clarinet
                | Electricguitar | Tenorhorn | Percussion | Rhythmguitar 
                | Hammondorgan   | Harpsichord | Cello    | Acousticguitar
                | Bassguitar     | Strings  | SteelDrum   | Vibraphone | Bongos
                | Steelguitar    | Horn     | Sitar | Barisaxophone | Accordion
                | Tambourine     | Kazoo
                | UnknownInstr String -- ^ a catch all description for 
                                      -- unrecognised instruments
       deriving (Show, Eq)
       
-- | Representing typical structural segementation labels
data Description = Chorus  | Intro | Outro | Bridge  | Interlude | Solo
                 | Fadeout | Fadein | Prechorus | Maintheme   | Keychange 
                 | Secondarytheme   | Ending    | PhraseTrans | Instrumental 
                 | Coda    | Transition | PreVerse   | Vocal  | Talking 
                 | TalkingEnd | Silence | Applause | Noise | SongEnd
                 | ModulationSeg | PreIntro
                 | Repeat Int
                 | Verse  (Maybe Int)
                 -- | a chord inserted by the posprocessing interpolation
                 | InterpolationInsert
                 -- | a catch all description for unrecognised descriptions
                 | UnknownAnno String 

       deriving (Show, Eq)


--------------------------------------------------------------------------------
-- Boundary Utilities
--------------------------------------------------------------------------------

-- | Returns the 'Label' of an 'Annotation'
getLabel :: Annotation -> Label
getLabel (Start l) = l
getLabel (End   l) = l

-- | Returns True if the 'Annotation' occurs at the start of a line
isStart :: Annotation -> Bool
isStart (Start _) = True
isStart (End   _) = False

-- | Returns True if the 'Annotation' annotates a structural segmentation label
isStruct :: Label -> Bool
isStruct (Struct _ _) = True
isStruct _            = False

-- | Returns True if the 'Annotation' 
isUnknown :: Annotation -> Bool
isUnknown s = case (getLabel s) of 
  (Instr (UnknownInstr _ )) -> True
  (Anno  (UnknownAnno  _ )) -> True
  _                         -> False

-- | Returns True if the 'Annotation' belongs to the set of 'Annotation's that
-- is typically found at the beginning and end of a piece. (This should hold 
-- for the complete Billboard dataset)
isBeginOrEndAnno :: Annotation -> Bool
isBeginOrEndAnno a = case getLabel a of
  (Anno Silence   ) -> True
  (Anno Noise     ) -> True
  (Anno Applause  ) -> True  
  (Anno TalkingEnd) -> True  
  (Anno Fadeout   ) -> True  
  (Anno PreIntro  ) -> True
  _                 -> False
 
isEndAnno :: Annotation -> Bool
isEndAnno (End (Anno SongEnd)) = True
isEndAnno (Start (Anno SongEnd)) = True
isEndAnno  _                   = False
 
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


