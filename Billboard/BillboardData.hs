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

module Billboard.BillboardData ( BBChord (..), isChange, hasAnnotations
                               , isStructSegStart, isNoneBBChord, noneBBChord
                               , BillboardData (..), Artist, Title, Meta (..)
                               , getBBChords, filterNoneChords
                               , showInMIREXFormat
                               ) where

-- HarmTrace stuff
import HarmTrace.Base.MusicRep  hiding (isNone)
import HarmTrace.Audio.ChordTypes (TimedData (..), getData, onset, offset)

import Billboard.BeatBar
import Billboard.Annotation ( Annotation (..), isStart, isStruct, getLabel )

import Data.List (partition)

-- | The 'BillboardData' datatype stores all information that has been extracted
-- from a Billboard chord annotation
data BillboardData = BillboardData { getTitle   :: Title    
                                   , getArtist  :: Artist 
                                   , getTimeSig :: TimeSig 
                                   , getKeyRoot :: Root    
                                   , getSong    :: [TimedData BBChord]
                                   } deriving Show

type Artist = String 
type Title  = String  
data Meta   = Metre   TimeSig 
            | KeyRoot Root    deriving Show


-- | We rap the 'HarmTrace.Base.MusicRep.Chord' datatype into a 'BBChord' type, 
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
    | rt == EQ = compare (toMode $ chordShorthand a) (toMode $ chordShorthand b)
    | otherwise  = rt where
        rt = compare (chordRoot a) (chordRoot b)

instance Eq BBChord where
  a == b = rootMode a == rootMode b where 
    rootMode (BBChord _ _ (Chord r sh _deg _loc _d)) = (r, toMode sh)


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

-- | Filters chords that have not annotated root or shorthand
filterNoneChords :: [BBChord] -> [BBChord]
filterNoneChords = filter (not . isNoneBBChord)
  
-- | Returns True if the 'BBChord' is a 'noneBBChord', i.e. has not root note 
-- and no shorthand
isNoneBBChord :: BBChord -> Bool
isNoneBBChord = isNoneChord . chord
  
-- | Returns True if the 'BBChord' has any 'Boundary's and false otherwise
hasAnnotations :: BBChord -> Bool
hasAnnotations = not . null . annotations

-- | Strips the time stamps from BillBoardData and concatnates all 'BBChords',
-- it also removes all NoneChords
getBBChords :: BillboardData -> [BBChord]
getBBChords = map getData . getSong 

-- | Shows the 'BillboardData' in MIREX format
showInMIREXFormat :: BillboardData -> String
showInMIREXFormat = concatMap showMIREX . getSong where
  showMIREX :: TimedData BBChord ->  String
  showMIREX c = show (onset c) ++ '\t' : show (offset c) 
                               ++ '\t' : (show . chord . getData $ c) ++ "\n"
