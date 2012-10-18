{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.BillboardData
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <W.B.deHaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set of datatypes for representing Billboard chord sequence data 
-- See: John Ashley Burgoyne, Jonathan Wild, Ichiro Fujinaga, 
-- /An Expert Ground-Truth Set for Audio Chord Recognition and Music Analysis/,
-- In: Proceedings of International Conference on Music Information Retrieval,
-- 2011. (<http://ismir2011.ismir.net/papers/OS8-1.pdf>) 
--------------------------------------------------------------------------------

module Billboard.BillboardData ( -- * The BillBoard data representation
                                 BillboardData (..)
                               , Meta (..)
                               , BBChord (..)                               
                               , Artist
                               , Title
                               , noneBBChord
                               -- * BillBoard data utilities
                               -- ** Data access
                               , getBBChords
                               , getBBChordsNoSilence
                               , addStart
                               , addEnd
                               , addLabel
                               , addStartEnd
                               , getStructAnn
                               -- ** Tests
                               , isStructSegStart
                               , isNoneBBChord
                               , isChange
                               , hasAnnotations
                               , isEnd
                               -- * Showing
                               , showInMIREXFormat
                               ) where

-- HarmTrace stuff
import HarmTrace.Base.MusicRep  hiding (isNone)
import HarmTrace.Base.MusicTime (TimedData (..), getData, onset, offset)

import Billboard.BeatBar
import Billboard.Annotation ( Annotation (..), isStart, isStruct
                            , getLabel, Label, isEndAnno
                            , isFirstChord, isLastChord)

import Data.List (partition)

-- | The 'BillboardData' datatype stores all information that has been extracted
-- from a Billboard chord annotation
data BillboardData = BillboardData { getTitle   :: Title    
                                   , getArtist  :: Artist 
                                   , getTimeSig :: TimeSig 
                                   , getKeyRoot :: Root    
                                   , getSong    :: [TimedData BBChord]
                                   } deriving Show

-- | Represents the artits of the piece
type Artist = String 
-- | Represents the title of the piece
type Title  = String  
-- | Represents other metadata of the piece, i.e. the time signature 
-- and key root
data Meta   = Metre   TimeSig 
            | KeyRoot Root    deriving Show


-- | We wrap the 'HarmTrace.Base.MusicRep.Chord' datatype into a 'BBChord' type, 
-- so that we can augment it with 'Annotation's and 'BeatWeight's.
data BBChord = BBChord { annotations :: [Annotation]
                       , weight      :: BeatWeight
                       , chord       :: Chord Root
                       } 

instance Show BBChord where 
  show (BBChord [] Beat  _c) = show Beat
  show (BBChord bd Beat  _c) = show Beat ++ show bd
  show (BBChord [] w      c) = show w ++ ' ' : show c
  show (BBChord bd w      c) = 
    let (srt, end) = partition isStart bd
    in  show w ++ concatMap show srt ++ ' ' : show c ++ ' ' : concatMap show end

-- for debugging
-- instance Show BBChord where
  -- show (BBChord NoBoundary  w _bt c) = show w ++ show c
  -- show (BBChord (Boundary a)w _bt c) = show w ++ show c ++ show a
  
instance Ord BBChord where
  compare (BBChord _ _ a) (BBChord _ _ b)
    | rt == EQ = compare (toTriad a) (toTriad b) -- N.B.toTriad can be expensive
    | otherwise  = rt where
        rt = compare (chordRoot a) (chordRoot b)

instance Eq BBChord where
  (BBChord _ _ a) == (BBChord _ _ b) = chordRoot a == chordRoot b && 
                                       toTriad   a == toTriad   b


--------------------------------------------------------------------------------
-- Some BBChord Utilities
--------------------------------------------------------------------------------
-- | A chord label with no root, shorthand or other information to represent
-- a none harmonic sections
noneBBChord :: BBChord
noneBBChord = BBChord [] Change noneLabel {duration =1}

-- | Returns True if the 'BBChord' represents a strating point of a stuctural
-- segement
isStructSegStart :: BBChord -> Bool-- look for segTypes that are Start and Struct
isStructSegStart = not . null . filter isStruct . map getLabel 
                              . filter isStart  . annotations 

-- | Returns True if the 'BBChord' represents a chord Change (must be set 
-- beforehand, only the 'BeatWeight' stored in the BBChord is examined)
isChange :: BBChord -> Bool
isChange c = case weight c of
  Change    -> True
  UnAligned -> error "BBChord.isChange: the BBChord is not beat aligned"
  _         -> False
  
-- | Returns True if the 'BBChord' is a 'noneBBChord', i.e. has not root note 
-- and no shorthand
isNoneBBChord :: BBChord -> Bool
isNoneBBChord = isNoneChord . chord

-- Returns True if this 'BBChord' is the last (N) chord of the song
isEnd :: BBChord -> Bool
isEnd c = isNoneBBChord c && hasAnnotation isEndAnno c 

-- | Returns True if the 'BBChord' has any 'Annotations's and false otherwise
hasAnnotations :: BBChord -> Bool
hasAnnotations = not . null . annotations

-- | Takes an 'Annotation' predicate and checks if it holds for a 'BBChord'
hasAnnotation :: (Annotation -> Bool) -> BBChord -> Bool
hasAnnotation f c = case annotations c of
  [] -> False
  a  -> or . map f $ a

-- | Adds a starting point of an 'Annotation' 'Label' to a 'BBChord'
addStart :: Label -> BBChord -> BBChord
addStart lab chrd = chrd { annotations = Start lab : annotations chrd }

-- | Adds an end point of an 'Annotation' 'Label' to a 'BBChord'
addEnd :: Label -> BBChord -> BBChord
addEnd lab chrd = chrd { annotations = End lab : annotations chrd }

-- | Adds both a start and an end 'Annotation' 'Label' to a 'BBChord'
addStartEnd :: Label -> BBChord -> BBChord
addStartEnd lab c = c { annotations = Start lab : End lab : annotations c }

-- | Annotates a sequence of 'BBChord's by adding a Start 'Label' 'Annotation'
-- at the first chord and an End 'Label' 'Annotation' at the last chord. The
-- remainder of the list remains untouched
addLabel :: Label -> [BBChord] -> [BBChord]
addLabel _ [ ] = [ ]
addLabel lab [c] = [addStart lab . addEnd lab $ c]
addLabel lab (c:cs)  = addStart lab c : foldr step [] cs where
  step :: BBChord -> [BBChord] -> [BBChord]
  step x [] = [addEnd lab x] -- add a label to the last element of the list
  step x xs = x : xs
  
-- | Strips the time stamps from BillBoardData and concatnates all 'BBChords'
getBBChords :: BillboardData -> [BBChord]
getBBChords = map getData . getSong

-- | Strips the time stamps from BillBoardData and concatnates all 'BBChords'
-- and removes the silence at the beginning and end of the song.
getBBChordsNoSilence :: BillboardData -> [BBChord]
getBBChordsNoSilence = removeSilence . getBBChords where

  -- Removes the Silence, Applause, and other non-harmonic None chords at the
  -- beginning and end of a piece
  removeSilence :: [BBChord] -> [BBChord]
  removeSilence = takeIncl  (not . hasAnnotation isLastChord ) .
                  dropWhile (not . hasAnnotation isFirstChord) 
                  
  -- Does exactly the same as 'takeWhile' but includes the element for which
  -- the predicate holds. For example takeIncl (< 3) [1..5] = [1, 2, 3]
  takeIncl :: (a -> Bool) -> [a] -> [a]
  takeIncl _ []     = [ ]
  takeIncl p (x:xs) 
     | p x          =  x : takeIncl p xs
     | otherwise    = [x]

-- | Returns the structural segmentation 'Annotation's, 
-- given a list of 'BBChord's
getStructAnn :: [BBChord] -> [Annotation]
getStructAnn = filter ( isStruct . getLabel ) . concatMap annotations
     
-- | Shows the 'BillboardData' in MIREX format, using only :maj, :min, :aug,
-- :dim, sus2, sus4, and ignoring all chord additions
showInMIREXFormat :: BillboardData -> String
showInMIREXFormat = concatMap showMIREX . getSong where

  showMIREX :: TimedData BBChord ->  String
  showMIREX c = show (onset c) ++ '\t' : show (offset c) 
                               ++ '\t' : (mirexBBChord . getData $ c) ++ "\n"
                               
  -- Categorises a chord as Major or Minor and shows it in Harte et al. syntax
  mirexBBChord :: BBChord -> String
  mirexBBChord bbc = let c = chord bbc 
                     in case (chordRoot c, chordShorthand c) of
                          ((Note _ N), None ) -> "N"
                          ((Note _ X), _    ) -> "X"
                          (r         , Sus2 ) -> show r ++ ":sus2"
                          (r         , Sus4 ) -> show r ++ ":sus4"
                          (r         , _    ) -> case toTriad c of
                                                   NoTriad ->  "X"
                                                   t   -> show r ++':' : show t
