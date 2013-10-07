{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.BillboardData
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
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
                               -- * Billboard data utilities
                               -- ** Data access
                               , getBBChords
                               , getBBChordsNoSilence
                               , addStart
                               , addEnd
                               , addLabel
                               , addStartEnd
                               , getStructAnn
                               , mergeAnnos
                               -- ** Tests
                               , isStructSegStart
                               , isNoneBBChord
                               , isChange
                               , hasAnnotations
                               , isEnd
                               -- ** Chord reduction
                               -- , reduceBBChords
                               -- , expandBBChords
                               , reduceTimedBBChords
                               , expandTimedBBChords
                               -- * Showing
                               , showInMIREXFormat
                               , showFullChord
                               ) where

-- HarmTrace stuff
import HarmTrace.Base.Chord
import HarmTrace.Base.Time     ( Timed (..), timedBT, getData
                                , onset, offset, concatTimed )

import Billboard.BeatBar
import Billboard.Annotation     ( Annotation (..), isStart, isStruct
                                , getLabel, Label, isEndAnno
                                , isFirstChord, isLastChord )
import Billboard.Internal       ( updateLast )

import Data.List                ( partition, sort )

-- | The 'BillboardData' datatype stores all information that has been extracted
-- from a Billboard chord annotation
data BillboardData = BillboardData { getTitle   :: Title    
                                   , getArtist  :: Artist 
                                   , getTimeSig :: TimeSig 
                                   , getKeyRoot :: Root    
                                   , getSong    :: [Timed BBChord]
                                   } deriving Show

-- | Represents the artists of the piece
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
                       } deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Some BBChord Utilities
--------------------------------------------------------------------------------
-- | A chord label with no root, shorthand or other information to represent
-- a none harmonic sections
noneBBChord :: BBChord
noneBBChord = BBChord [] Change NoChord

-- | Returns True if the 'BBChord' represents a starting point of a structural
-- segment
isStructSegStart :: BBChord -> Bool-- look for segTypes that are Start and Struct
isStructSegStart = not . null . filter isStruct . map getLabel 
                              . filter isStart  . annotations 

-- | Returns True if the 'BBChord' represents a chord Change (must be set 
-- beforehand, only the 'BeatWeight' stored in the 'BBChord' is examined)
isChange :: BBChord -> Bool
isChange c = case weight c of
  Change    -> True
  UnAligned -> error "BBChord.isChange: the BBChord is not beat aligned"
  _         -> False
  
-- | Returns True if the 'BBChord' is a 'noneBBChord', i.e. has not root note 
-- and no shorthand
isNoneBBChord :: BBChord -> Bool
isNoneBBChord = isNoneChord . chord

-- | Returns True if this 'BBChord' is the last (N) chord of the song
isEnd :: BBChord -> Bool
isEnd c = isNoneBBChord c && hasAnnotation isEndAnno c 

-- | Returns True if the 'BBChord' has any 'Annotations's and False otherwise
hasAnnotations :: BBChord -> Bool
hasAnnotations = not . null . annotations

-- | Takes an 'Annotation' predicate and checks if it holds for a 'BBChord'
hasAnnotation :: (Annotation -> Bool) -> BBChord -> Bool
hasAnnotation f c = case annotations c of
  [] -> False
  a  -> or . map f $ a
  
-- adds 'Annotation's to a 'BBChord' (and sorts the annotations)
mergeAnnos :: [Annotation] -> BBChord -> BBChord
mergeAnnos b a = a { annotations = sort (annotations a ++ b) }

-- | Adds a starting point of an 'Annotation' 'Label' to a 'BBChord'
addStart :: Label -> BBChord -> BBChord
addStart lab = mergeAnnos [Start lab]

-- | Adds an end point of an 'Annotation' 'Label' to a 'BBChord'
addEnd :: Label -> BBChord -> BBChord
addEnd lab = mergeAnnos [End lab]

-- | Adds both a start and an end 'Annotation' 'Label' to a 'BBChord'
addStartEnd :: Label -> BBChord -> BBChord
addStartEnd lab = mergeAnnos [Start lab, End lab]

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

-- | Strips the time stamps from BillBoardData and concatenates all 'BBChords'
getBBChords :: BillboardData -> [BBChord]
getBBChords = map getData . getSong

-- | Strips the time stamps from BillBoardData and concatenates all 'BBChords'
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
-- given a 'BBChord'
getStructAnn :: BBChord -> [Annotation]
getStructAnn = filter ( isStruct . getLabel ) . annotations

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | the inverse of 'reduceTimedBBChords', expanding a chord to it original 
-- representation
expandTimedBBChords :: [Timed BBChord] -> [Timed BBChord]
expandTimedBBChords = concatMap replic where

  replic :: Timed BBChord -> [Timed BBChord]
  replic (Timed c ts) = 
    let (s,e) = partition isStart (annotations c)
        reps = repeat c { weight = Beat, annotations = [] }
        
    in  updateLast (fmap (mergeAnnos e))
      $ zipWith3 timedBT (c { annotations = s } : reps) ts (tail ts)

-- | The inverse function of 'expandTimedBBChords': given a list of 
-- 'Timed BBChords', all subsequent /x/ 'BBChords' with the same 
-- label are grouped into one 'Timed BBChord'. N.B. 
--
-- >>> expandTimedBBChords (reduceTimedBBChords cs) = cs :: [Timed BBChord]
-- 
-- also,
--
-- >>> (expandTimedBBChords cs) = cs
--
-- and,
--
-- >>> reduceTimedBBChords (reduceTimedBBChords cs) = (reduceTimedBBChords cs)
--
-- hold. This has been tested on the first tranche of 649 Billboard songs.
reduceTimedBBChords :: [Timed BBChord] -> [Timed BBChord]
reduceTimedBBChords = foldr groupT [] where

   groupT :: Timed BBChord -> [Timed BBChord] -> [Timed BBChord]
   groupT c [] = [c]
   groupT tc@(Timed c _ ) (th@(Timed h _ ) : t)
     | c `bbChordEq` h = concatTimed ( mergeAnnos (annotations h) c ) tc th : t
     | otherwise       = tc : th : t


-- keep groupBBChord and expandChordDur "inverseable" we use a more strict
-- 'BBChord' equality  
bbChordEq :: BBChord -> BBChord -> Bool
bbChordEq (BBChord anA btA cA) (BBChord anB btB cB) = 
  cA   ==      cB  &&    -- as of HarmTrace-Base-1.4 we have derive chord Eq
  anA `annEq`  anB &&
  btA `beatEq` btB where
  
    annEq :: [Annotation] -> [Annotation] -> Bool
    -- annEq [] [] = True
    annEq _  [] = True
    annEq _  e  = onlyEndAnns e
    -- annEq a  b  = a == b
    
    -- returns True if the Annotations are only of the 'End' type or empty
    onlyEndAnns :: [Annotation] -> Bool
    onlyEndAnns []    = True
    onlyEndAnns (h:t) = not (isStart h) && onlyEndAnns t
    
    beatEq :: BeatWeight -> BeatWeight -> Bool  
    beatEq LineStart Beat       = True
    beatEq Bar       Beat       = True
    beatEq Change    Beat       = True
    beatEq Bar       Bar        = False
    beatEq Change    Change     = False
    beatEq LineStart LineStart  = False
    beatEq a         b          = a == b
--------------------------------------------------------------------------------
-- Printing chord sequences
--------------------------------------------------------------------------------

-- | Shows the chord sequence in the 'BillboardData'
showFullChord :: ([Timed BBChord] -> [Timed BBChord]) 
              -> BillboardData -> String
showFullChord redf = concatMap (showLine (show . chord)) . redf . getSong 

-- | Shows the 'BillboardData' in MIREX format, using only :maj, :min, :aug,
-- :dim, sus2, sus4, and ignoring all chord additions
showInMIREXFormat :: ([Timed BBChord] -> [Timed BBChord]) 
                  -> BillboardData -> String
showInMIREXFormat redf = concatMap (showLine mirexBBChord) . redf . getSong 

-- | Shows a 'Timed' 'BBChord' in MIREX triadic format, using only :maj, 
-- :min, :aug, :dim, sus2, sus4, and ignoring all chord additions 
showLine ::  (BBChord -> String) -> Timed BBChord ->  String
showLine shwf c = show (onset c) ++ '\t' :  show (offset c) 
                                 ++ '\t' : (shwf . getData $ c) ++ "\n" 
                               
-- Categorises a chord as Major or Minor and shows it in Harte et al. syntax
mirexBBChord :: BBChord -> String
mirexBBChord bbc = case chord bbc of
                     NoChord    -> "N"
                     UndefChord -> "X"
                     c -> case toTriad c of
                            NoTriad -> case chordShorthand c of
                                         Sus2 -> show (chordRoot c) ++ ":sus2"
                                         Sus4 -> show (chordRoot c) ++ ":sus4"
                                         _    -> "X"
                            t      ->            show (chordRoot c) ++':' : show t
                            
                   -- in case (chordRoot x, chordShorthand x) of
                        -- ((Note _ N), None ) -> "N"
                        -- ((Note _ X), _    ) -> "X"
                        -- (r         , Sus2 ) -> show r ++ ":sus2"
                        -- (r         , Sus4 ) -> show r ++ ":sus4"
                        -- (r         , _    ) -> case toTriad x of
                                                 -- NoTriad ->  "X"
                                                 -- t   -> show r ++':' : show t
