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
                               -- , getDuration
                               , getStructAnn
                               -- , setChordIxsT
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
                                
import Data.List                ( partition )

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
                       } deriving Show

instance Ord BBChord where
  compare (BBChord _ _ a) (BBChord _ _ b)
    | rt == EQ = compare (toTriad a) (toTriad b) -- N.B.toTriad can be expensive
    | otherwise  = rt where
        rt = compare (chordRoot a) (chordRoot b)

-- TODO replace by derived EQ, use a specific EQ where needed.
instance Eq BBChord where
  (BBChord _ _ a) == (BBChord _ _ b) = chordRoot a == chordRoot b && 
                                       toTriad   a == toTriad   b


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

-- | Sets the indexes of a list of 'Timed' 'BBChord's (starting at 0)
-- setChordIxsT :: [Timed BBChord] -> [Timed BBChord]
-- setChordIxsT cs = zipWith (fmap . flip setChordIx) [0..] cs   
  
-- -- | Sets the indexes of a list of 'BBChords' (starting at 0)
-- setChordIxs :: [BBChord] -> [BBChord]
-- setChordIxs cs = zipWith setChordIx cs [0..]
  
-- -- sets the index of a 'BBChord' (should not be exported)
-- setChordIx :: BBChord -> Int -> BBChord 
-- setChordIx rc i = let x = chord rc in rc {chord = x {getLoc = i} }

-- | Returns the duration of the chord (the unit of the duration can be 
-- application dependent, but will generally be measured in eighth notes)
-- If the data comes directly from the parser the duration will be 1 for
-- all 'BBChord's. However, if it has been \reduced\ with 'reduceBBChords'
-- the duration will be the number of consecutive tatum units.
-- getDuration :: BBChord -> Int
-- getDuration = duration . chord

-- sets the duration of an 'BBChord'
-- setDuration :: BBChord -> Int -> BBChord
-- setDuration c i = let x = chord c in c { chord = x { duration = i } }
  
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

-- | Given a list of 'BBChord's that have a certain duration (i.e. the number of 
-- beats that the chord should sound), every 'BBChord' is replaced by /x/ 
-- 'BBChord's with the same properties, but whit a duration of 1 beat, where /x/ 
-- is the duration of the original 'BBChord'
-- expandBBChords :: [BBChord] -> [BBChord]
-- expandBBChords = setChordIxs . concatMap replic where
  -- replic c = let x = setDuration c 1 
             -- in  x : replicate (pred . duration $ chord c) 
                               -- x { weight = Beat, annotations = []}
                               
-- | The inverse function of 'expandChordDur': given a list of 'BBChords' that 
-- all have a duration of 1 beat, all subsequent /x/ 'BBChords' with the same 
-- label are grouped into one 'BBChord' with durations /x/. N.B. 
--
-- >>> expandBBChords (reduceBBChords cs) = cs
-- 
-- also,
--
-- >>> (expandBBChords cs) = cs
--
-- and,
--
-- >>> reduceBBChords (reduceBBChords cs) = (reduceBBChords cs)
--
-- hold. This has been tested on the first tranche of 649 Billboard songs
-- reduceBBChords :: [BBChord] -> [BBChord]
-- reduceBBChords = setChordIxs . foldr group []  where
  
  -- group :: BBChord -> [BBChord] -> [BBChord]
  -- group c [] = [c]
  -- group c (h:t)
    -- | c `bbChordEq` h  = setDuration c (succ . duration $ chord h): t
    -- | otherwise        = c : h : t

-- | Similar to 'expandBBChords' the inverse of 'reduceTimedBBChords'
expandTimedBBChords :: [Timed BBChord] -> [Timed BBChord]
expandTimedBBChords = concatMap replic where

  replic :: Timed BBChord -> [Timed BBChord]
  replic (Timed c ts) = 
    let (s,e) = partition isStart (annotations c)
        reps = repeat c { weight = Beat, annotations = [] }
        
        -- places the end annotations at the end of the list
        updateEnd :: [Timed BBChord] -> [Timed BBChord]
        updateEnd [ ]           = [ ]
        updateEnd [ Timed d x ] = [ Timed d { annotations = e } x ]
        updateEnd (h : t)       = h : updateEnd t
        
    in  updateEnd $ zipWith3 timedBT (c { annotations = s } : reps) ts (tail ts)

-- | Returns the reduced chord sequences, where repeated chords are merged
-- into one 'BBChord', similar to 'reduceBBChords', but then wrapped in a 
-- 'Timed' type.
reduceTimedBBChords :: [Timed BBChord] -> [Timed BBChord]
reduceTimedBBChords = foldr groupT [] where

   groupT :: Timed BBChord -> [Timed BBChord] -> [Timed BBChord]
   groupT c [] = [c]
   groupT tc@(Timed c _ ) (th@(Timed h _ ) : t)
     | c `bbChordEq` h = concatTimed (mergeChord c h) tc th : t
     | otherwise       = tc : th : t
     
   mergeChord :: BBChord -> BBChord -> BBChord
   mergeChord a b = a { annotations = annotations a ++ annotations b }


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
showFullChord redf = concatMap (showLine show) . redf . getSong 

-- | Shows the chord sequence in the 'BillboardData'
showLabChord :: ([Timed BBChord] -> [Timed BBChord]) 
              -> BillboardData -> String
showLabChord redf = concatMap (showLine (show . chord)) . redf . getSong 

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
