{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE EmptyDataDecls                 #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE GADTs                          #-}

module HarmTrace.Base.MusicRep where
  
import Data.Maybe
import Data.List (elemIndex, intersperse, intercalate)  

-- harmtrace specific dependencies
-- import Control.DeepSeq
-- import HarmTrace.HAnTree.Binary
-- import Generics.Instant.TH
-- import Data.Binary
  
--------------------------------------------------------------------------------
-- Representing musical information at the value level
--------------------------------------------------------------------------------

-- Keys (at the value level)
data Key  = Key { keyRoot :: Root, keyMode :: Mode } deriving (Eq, Ord)
data Mode = MajMode | MinMode deriving (Eq, Ord)

-- instance NFData Mode where
  -- rnf MinMode = ()
  -- rnf MajMode = ()
  
type ChordLabel   = Chord Root
type ChordDegree  = Chord ScaleDegree

-- the representation for a single tokenized chord 
data Chord a = Chord { chordRoot        :: a
                     , chordShorthand   :: Shorthand
                     , chordAdditions   :: [Addition]
                     , getLoc           :: Int -- the index of the chord  
                     , duration         :: Int -- in the list of tokens
                     }

data Class = Class ClassType Shorthand

data ClassType = MajClass | MinClass | DomClass | DimClass | NoClass
  deriving (Eq)

data Shorthand = -- Triad chords
                 Maj | Min | Dim | Aug
                 -- Seventh chords
               | Maj7 | Min7 | Sev | Dim7 | HDim7 | MinMaj7
                 -- Sixth chords
               | Maj6 | Min6
                 -- Extended chords
               | Nin | Maj9 | Min9
                 -- Suspended chords
               | Sus4 | Sus2
                 -- Power chords
               | Five
                 -- In some cases there is no chord a certain position
                 -- This is especially important for the chroma processing
               | None
                 -- ambiguous shorthands in billboard collection
               | Eleven | Thirteen | Min11 | Maj13 | Min13
               
  deriving (Show, Eq, Enum, Bounded) 


-- Key relative scale degrees to abstract from the absolute Root notes
type ScaleDegree = Note DiatonicDegree

data DiatonicDegree = I | II | III | IV | V | VI | VII | Imp
  deriving (Show, Eq, Enum, Ord, Bounded)

-- Representing absolute root notes  
type Root = Note DiatonicNatural
  
data DiatonicNatural =  C | D | E | F | G | A | B | N | X -- N is for no root, X is for MIREX
  deriving (Show, Eq, Enum, Ord, Bounded)
  
-- Intervals for additonal chord notes    
data Addition = Add   (Note Interval)
              | NoAdd (Note Interval) deriving Eq
  
data Interval = I1  | I2  | I3  | I4 | I5 | I6 | I7 | I8 | I9 | I10 
              | I11 | I12 | I13 
  deriving (Eq, Enum, Ord, Bounded)     
  
data Note a = Note (Maybe Modifier) a   deriving (Eq, Ord) 
  
data Modifier = Sh | Fl | SS | FF -- Sharp, flat, double sharp, double flat
  deriving (Eq, Ord)

data Triad = MajTriad | MinTriad | AugTriad | DimTriad | NoTriad deriving Eq
  
--------------------------------------------------------------------------------
-- Instances for the general music datatypes
--------------------------------------------------------------------------------   

instance Show Key where
  show (Key r m) = show r ++ show m
    
instance Show Mode where
  show MajMode = ""
  show MinMode = "m"  
  
instance Eq a => Eq (Chord a) where
  (Chord ra sha dega _loc _d) == (Chord rb shb degb _locb _db) 
     = ra == rb && sha == shb && dega == degb 
  
instance (Show a) => Show (Chord a) where
  show (Chord r sh deg _loc _d) =  show r ++ ':' : show sh 
                            ++ (if not (null deg) then showAdds deg else "")
                            -- ++ '_' : show loc ++ ':' : show d

showAdds :: Show a => [a] -> String                                
showAdds x = '(' : intercalate "," (map show x) ++ ")"
     
instance Show Class where show (Class ct _) = show ct
                            
instance Show ClassType where
  show MajClass = ""
  show MinClass = "m"
  show DomClass = "7"
  show DimClass = "0"
  show NoClass  = "N"

instance (Show a) => Show (Note a) where
  show (Note m interval) = show interval ++ maybe "" show m

instance Show Interval where
  show a = show . ((!!) ([1..13]::[Integer])) 
                . fromJust $ elemIndex a [minBound..]
   
  
instance Show Modifier where 
  show Sh = "#"
  show Fl = "b"
  show SS = "##"
  show FF = "bb"     

instance Show Addition where
  show (Add   n) = show n
  show (NoAdd n) = '*' : show n
  
-- for showing additional additions
showAdditions :: [Addition] -> String
showAdditions a 
  | null a    = ""
  | otherwise = "(" ++ concat (intersperse ","  (map show a)) ++ ")"           

instance Show Triad where
  show MajTriad = "maj"
  show MinTriad = "min"
  show AugTriad = "aug"
  show DimTriad = "dim"
  show NoTriad  = "NoTriad"
  
--------------------------------------------------------------------------------
-- Utils      
--------------------------------------------------------------------------------

isNone :: Root -> Bool
isNone (Note _ N) = True
isNone  _         = False

noneLabel :: ChordLabel
noneLabel = (Chord (Note Nothing N) None [] 0 0)

isNoneChord :: ChordLabel -> Bool
isNoneChord (Chord (Note _ N) _ _ _ _) = True
isNoneChord _                          = False

isUnknown :: Root -> Bool
isUnknown (Note _ X) = True
isUnknown _          = False

unknownLabel :: ChordLabel
unknownLabel = (Chord (Note Nothing X) None [] 0 0)

isUnknownChord :: ChordLabel -> Bool
isUnknownChord (Chord (Note _ X) _ _ _ _) = True
isUnknownChord (Chord (Note _ N) _ _ _ _) = False -- known to be NoneChord
isUnknownChord (Chord _ None _ _ _)       = True
isUnknownChord _                          = False

toClassType :: Shorthand -> ClassType
toClassType sh 
  | sh `elem` [Maj,Maj7,Maj6,Maj9,MinMaj7,Five,Sus4,Sus2] = MajClass
  | sh `elem` [Min,Min7,Min6,Min9,HDim7] = MinClass
  | sh `elem` [Sev,Nin,Aug] = DomClass
  | sh `elem` [Dim,Dim7] = DimClass 
  | otherwise = error 
      ("HarmTrace.Base.MusicRep.toClassType: unknown shorthand: " ++ show sh)

-- analyses a list of Degrees and assigns a shortHand i.e. Chord Class        
-- analyseDegsTriad :: [Addition] -> Shorthand        
-- analyseDegsTriad d 
  -- -- minor third
  -- | (Note (Just Fl) I3) `elem` d = 
  -- -- major third
  -- | (Note  Nothing  I3) `elem` d = Maj
  
  
  
  
  -- | (Note (Just Fl) I7)  `elem` d = Sev
  -- | (Note  Nothing  I7)  `elem` d = Maj7
  -- | (Note (Just Fl) I9)  `elem` d = Sev
  -- | (Note (Just Sh) I9)  `elem` d = Sev
  -- | (Note  Nothing  I11) `elem` d = Sev
  -- | (Note (Just Sh) I11) `elem` d = Sev
  -- | (Note (Just Fl) I13) `elem` d = Sev
  -- | (Note  Nothing  I13) `elem` d = Sev
  -- | otherwise                     = Maj
   
-- hasMajThird
   
      
-- | Converts a 'Shorthand' to a 'Triad' 
-- N.B. this function should not be exported because the shorthand alone cannot
-- determine the triad 
toTriad :: Shorthand -> Triad     
toTriad Maj     = MajTriad
toTriad Min     = MinTriad
toTriad Dim     = DimTriad
toTriad Aug     = AugTriad
toTriad Maj7    = MajTriad
toTriad Min7    = MinTriad
toTriad Sev     = MajTriad
toTriad Dim7    = MinTriad
toTriad HDim7   = MinTriad
toTriad MinMaj7 = MinTriad
toTriad Maj6    = MajTriad 
toTriad Min6    = MinTriad
toTriad Nin     = MajTriad
toTriad Maj9    = MajTriad
toTriad Min9    = MinTriad
toTriad Five    = NoTriad
toTriad Sus2    = NoTriad
toTriad Sus4    = NoTriad
toTriad None    = NoTriad
-- additional Billboard shorthands
toTriad Min11    = MinTriad
toTriad Min13    = MinTriad
toTriad Maj13    = MajTriad
toTriad Eleven   = MajTriad
toTriad Thirteen = MajTriad
-- toTriad m        = 
  -- error ("HarmTrace.Base.MusicRep.toMode: unkown shorthand: " ++ show m)      
      
-- | Converts a 'Shorthand' to a 'Mode'
toMode :: Shorthand -> Mode     
toMode sh = case toTriad sh of
  MajTriad -> MajMode
  MinTriad -> MinMode
  m        -> error (  "HarmTrace.Base.MusicRep.toMode: cannot convert "
                    ++ " shorthand to mode: " ++ show m)

-- | Converts a 'Shorthand' to either a 'MajClass', 'MinClass' or 'NoClass' 
-- 'ClassType'.
toMajMin :: Shorthand -> ClassType
toMajMin sh = case toTriad sh of
  MajTriad -> MajClass
  MinTriad -> MinClass
  AugTriad -> MajClass
  DimTriad -> MinClass
  NoTriad  -> NoClass


--------------------------------------------------------------------------------
-- Value Level Scale Degree Transposition
-------------------------------------------------------------------------------- 
    
-- Chord root shorthand degrees location duration
toChordDegree :: Key -> ChordLabel -> ChordDegree
toChordDegree k (Chord r sh degs loc d) = 
                 Chord (toScaleDegree k r) sh degs loc d    
    
toScaleDegree :: Key -> Root -> ScaleDegree
toScaleDegree _ n@(Note _ N) = 
  error ("HarmTrace.Base.MusicRep.toScaleDegree: cannot transpose" ++ show n)
toScaleDegree (Key kr _) cr  = -- Note Nothing I
  scaleDegrees!!(((diaNatToSemi cr) - (diaNatToSemi kr)) `mod` 12)

-- transposes a degree with sem semitones up
transposeSem :: ScaleDegree -> Int -> ScaleDegree
transposeSem deg sem = scaleDegrees!!((sem + (diaDegToSemi deg)) `mod` 12) where

-- gives the semitone value [0,11] of a Degree, e.g. F# = 6
diaDegToSemi :: ScaleDegree -> Int
diaDegToSemi n@(Note _ Imp) = 
  error ("HarmTrace.Base.MusicRep.diaDegToSemi: no semitone for" ++ show n)
diaDegToSemi   (Note m deg) = 
  ([0,2,4,5,7,9,11] !! (fromJust $ elemIndex deg [minBound..])) + (modToSemi m) 
  -- probably solved better this way
  -- ([0,2,4,5,7,9,11] !! (fromEnum deg)) + modToSemi m) 
  
diaNatToSemi :: Root -> Int
diaNatToSemi n@(Note _ N  ) = 
  error ("HarmTrace.Base.MusicRep.diaDegToSemi: no semitone for" ++ show n)
diaNatToSemi   (Note m nat) = 
  ([0,2,4,5,7,9,11] !! (fromJust $ elemIndex nat [minBound..])) + (modToSemi m) 


-- transforms type-level modifiers to semitones (Int values)
modToSemi :: Maybe Modifier -> Int
modToSemi  Nothing  =  0
modToSemi (Just Sh) =  1
modToSemi (Just Fl) = -1
modToSemi (Just SS) =  2
modToSemi (Just FF) = -2
           
scaleDegrees ::[ScaleDegree]  
scaleDegrees = [ Note  Nothing   I
               , Note  (Just Fl) II
               , Note  Nothing   II
               , Note  (Just Fl) III
               , Note  Nothing   III
               , Note  Nothing   IV
               , Note  (Just Sh) IV
               , Note  Nothing   V
               , Note  (Just Fl) VI
               , Note  Nothing   VI
               , Note  (Just Fl) VII
               , Note  Nothing   VII
               ]

--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

-- deriveAllL [''Note, ''DiatonicDegree
           -- , ''Mode, ''Chord, ''DiatonicNatural, ''ClassType
           -- , ''Modifier, ''Shorthand, ''Interval]

-- instance (Binary a) => Binary (Note a) where
  -- put = putDefault
  -- get = getDefault
-- instance Binary DiatonicDegree where
  -- put = putDefault
  -- get = getDefault
-- instance Binary Mode where
  -- put = putDefault
  -- get = getDefault
-- instance (Binary a) => Binary (Chord a) where
  -- put = putDefault
  -- get = getDefault
-- instance Binary DiatonicNatural where
  -- put = putDefault
  -- get = getDefault
-- instance Binary ClassType where
  -- put = putDefault
  -- get = getDefault
-- instance Binary Modifier where
  -- put = putDefault
  -- get = getDefault
-- instance Binary Shorthand where
  -- put = putDefault
  -- get = getDefault
-- instance Binary Interval where
  -- put = putDefault
  -- get = getDefault
