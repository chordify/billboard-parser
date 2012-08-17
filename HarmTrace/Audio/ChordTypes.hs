{-# LANGUAGE TypeSynonymInstances             #-}
{-# LANGUAGE FlexibleInstances                #-}
{-# LANGUAGE KindSignatures                   #-}
{-# LANGUAGE DeriveFunctor                    #-}

module HarmTrace.Audio.ChordTypes where
             
import HarmTrace.Base.MusicRep
import Text.Printf (printf)
-- import Control.DeepSeq

--------------------------------------------------------------------------------
-- High-level structure
--------------------------------------------------------------------------------

type ChordBeatAnnotation = [BeatTimedData ChordLabel]

-- the standard evaluation format of a chord annotation consists of a
-- list with chords and segment boundaries
type ChordAnnotation = [TimedData ChordLabel]

data TimedData a = TimedData a NumData NumData deriving Functor

-- a datatype that wraps around an arbitrary datatype adding (in this order)
-- a 'Beat', an onset, and an offset
data BeatTimedData a = BeatTimedData a Beat NumData NumData deriving Functor

-- an alternative constructor for a BeatTimedData using two BeatBar datatypes
-- instead of a 'Beat' and two 'NumData's
beatTimedData :: a -> BeatBar -> BeatBar -> BeatTimedData a
beatTimedData a on off = let (onnum,onbt) = beatBar on 
                         in  BeatTimedData a onbt onnum (fst $ beatBar off)

getBeat :: BeatTimedData a -> Beat
getBeat (BeatTimedData _ b _ _) = b

-- clusering propchords in a collection of chords that share a key
data ProbChordSeg = Segment { segKey    :: Key 
                            , segChords :: [BeatTimedData [ProbChord]] }
  
-- combining a chord with a probability
data ProbChord = ProbChord {chordLab :: ChordLabel, prob :: NumData}

-- a chord candidate: an intermediate datatype that matches shorthand, 
-- chord structure and root note (plus inversion)
data ChordCand = ChordCand { originalRootCC   :: Root
                           , inversionRootCC  :: Root
                           , shorthardCC      :: Shorthand
                           , chordStructCC    :: ChordStruct }
  deriving Show
  
type ChordStruct = [NumData] 

-- For now we fix the number of available beats to four, because this is also
-- hard-coded into the bar and beat-tracker.
data Beat = One | Two | Three | Four deriving (Eq, Enum)

-- We still assume 4/4, in the future this function should also have the
-- meter as an argument
nextBeat, prevBeat :: Beat -> Beat 
nextBeat Four = One
nextBeat b    = succ b

prevBeat One  = Four
prevBeat b    = pred b

-- an iterable list of Roots
chromaPC ::[Root]  
chromaPC = [ Note Nothing   C
           , Note (Just Fl) D
           , Note Nothing   D
           , Note (Just Fl) E
           , Note Nothing   E
           , Note Nothing   F
           , Note (Just Sh) F
           , Note Nothing   G
           , Note (Just Fl) A
           , Note Nothing   A
           , Note (Just Fl) B
           , Note Nothing   B
           ]

class Timed t where
  getData   :: t a -> a
  onset     :: t a -> NumData
  offset    :: t a -> NumData
  setData   :: t a -> b       -> t b
  setOnset  :: t a -> NumData -> t a
  setOffset :: t a -> NumData -> t a
           
instance Timed TimedData where
  getData   (TimedData d _  _  ) = d
  onset     (TimedData _ on _  ) = on
  offset    (TimedData _ _  off) = off
  setData   (TimedData _ on off) d   = (TimedData d on off)
  setOnset  (TimedData d _  off) on  = (TimedData d on off)
  setOffset (TimedData d on _  ) off = (TimedData d on off)
  
instance Timed BeatTimedData where
  getData   (BeatTimedData d _ _  _  ) = d
  onset     (BeatTimedData _ _ on _  ) = on
  offset    (BeatTimedData _ _ _  off) = off
  setData   (BeatTimedData _ b on off) d   = (BeatTimedData d b on off)
  setOnset  (BeatTimedData d b _  off) on  = (BeatTimedData d b on off)
  setOffset (BeatTimedData d b on _  ) off = (BeatTimedData d b on off)

--------------------------------------------------------------------------------
-- NFData instances
-------------------------------------------------------------------------------- 

-- Simplified
-- instance NFData (TimedData ChordLabel) where
  -- rnf (TimedData a b c) = a `seq` rnf b `seq` rnf c

-- instance NFData Beat where  
  -- rnf One   = ()
  -- rnf Two   = ()
  -- rnf Three = ()
  -- rnf Four  = ()

--------------------------------------------------------------------------------
-- Instances of high-level datastructure
-------------------------------------------------------------------------------- 

instance Eq (ProbChord) where
  a == b = chordLab a == chordLab b

-- TODO remove line-endings from show instances

instance Show (ProbChord) where 
  show (ProbChord (Chord r sh _ _ _) p) = 
    show r ++ ':' : show sh ++ ':' : printf "%.2f" p  

instance Show a => Show (TimedData a) where 
  show (TimedData bk s l) = show bk ++ " (" ++ show s ++ ':' : show l ++ ")\n"

instance Show ProbChordSeg where
  show pc = concatMap (\x -> (show $ segKey pc) ++ ' ' : show x) (segChords pc)

instance Show Beat where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"

instance Show BeatBar where
  show = show . beatBar
  
instance Show a => Show (BeatTimedData a) where
  show (BeatTimedData dat bt on off) = 
    show bt ++ ';' : show dat ++ ';' : show on ++ ';' : show off ++ "\n"
    
--------------------------------------------------------------------------------
-- numerical data representation
--------------------------------------------------------------------------------

data AudioFeat = AudioFeat ChordinoData BeatBarTrackData KeyStrengthData 

type ChordinoData = [ChordinoLine]  
data ChordinoLine = ChordinoLine 
  { time ::  NumData 
  , bass :: [NumData]   -- each of the lists has always 12 elements 
  , treb :: [NumData]   -- A, Bb, B, C, Db, D, Eb, E, F, F#, G, Ab 
  } deriving (Eq, Show) -- and is shifted 3 positions to match C, Db, .., B
  
type KeyStrengthData = ChordinoData  

type BeatTrackerData = [NumData]

newtype BeatBar = BeatBar {beatBar :: (NumData, Beat)} deriving Eq

type BeatBarTrackData = [BeatBar]

type NumData = Double

type BeatChroma = [[ChordinoLine]] -- one list per beat

-- we compare based on the timestamp only
instance Ord BeatBar where
  compare (BeatBar (b1,_)) (BeatBar (b2,_)) = compare b1 b2

--------------------------------------------------------------------------------
-- Some type conversion utitlities
--------------------------------------------------------------------------------

getBeatTrack :: BeatBarTrackData -> BeatTrackerData
getBeatTrack = map (fst . beatBar)

-- adds 'Beat' information to a 'Timed' datatype
setBeat :: Timed t => t a -> Beat -> BeatTimedData a
setBeat tdat bt = BeatTimedData (getData tdat) bt (onset tdat) (offset tdat)

-- converts a list of 'BeatTimedData's into a list of 'TimedData's
dumpBeats :: [BeatTimedData a] -> [TimedData a]
dumpBeats = map dumpBeat

-- converts a 'BeatTimedData' into a 'TimedData'
dumpBeat :: BeatTimedData a -> TimedData a
dumpBeat (BeatTimedData dat _bt on off) = TimedData dat on off

-- returns the timestamp of a 'BeatBar'
timeStamp :: BeatBar -> NumData
timeStamp = fst . beatBar

-- returns the 'Beat' of a 'BeatBar'
beat :: BeatBar -> Beat
beat = snd . beatBar
