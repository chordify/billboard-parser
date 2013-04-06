{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main ( main ) where

import Prelude hiding (sum)

import qualified Data.Map as M
import qualified Data.Vector as V ( fromList, map)
-- import Data.Vector                ( Vector )

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..), reduceTimedBBChords )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Monoid (mappend)
import Data.List (nub ) -- , intercalate)
import Data.Binary (encodeFile)

import ChordTrack.Audio.Viterbi
import ChordTrack.Audio.VectorNumerics

type Transitions = M.Map (ChordLabel, ChordLabel) Prob
type InitCount   = M.Map ChordLabel Int

allRoots :: [Root]
-- To keep things simple, we generate a lot of notes, and then simplify them
allRoots = nub . map simplifyRoot $
           [ Note m r | m <- [Nothing, Just Sh, Just Fl]
                      , r <- [C,D,E,F,G,A,B] ]

allChords :: [ChordLabel]
allChords = shortChord (Note Nothing N) None :
          [ shortChord r sh | sh <- [Maj,Min], r <- allRoots ]

emptyTransitions :: Transitions
emptyTransitions = M.fromList 
                 $ [ ((c1,c2),0.0) | c1 <- allChords, c2 <- allChords ]          
                   
-- Get the chords, throw away the rest
stripData :: BillboardData -> [ChordLabel]
stripData = filter isGood . map (simplify . chord . getData)
                          . reduceTimedBBChords . getSong where
                          
  simplify :: ChordLabel -> ChordLabel
  simplify c = toMajMinChord c { chordRoot = simplifyRoot (chordRoot c)
                               , chordAdditions = []
                               , getLoc = 0, duration = 1 }
  
  isGood :: ChordLabel -> Bool
  isGood (Chord (Note _ N) None _ _ _) = True
  isGood (Chord (Note _ X) _    _ _ _) = False
  isGood (Chord _          None _ _ _) = False -- for chords without a triad
  isGood _                             = True

-- Process all files in the dir
statsAll :: FilePath -> IO Transitions
statsAll fp = do files <- getBBFiles fp
                 ts    <- foldM doFiles emptyTransitions files
                 
                 -- uncomment to show probability instead of counts
                 -- let nrTs = M.foldr (+) 0 ts
                 -- return $ M.map (/ nrTs) ts where
                 return ts where
  
  doFiles :: Transitions -> (FilePath, Int) -> IO Transitions
  doFiles ts (d,_) = readFile d >>= return . statsOne . fst . parseBillboard where
    
    statsOne :: BillboardData -> Transitions
    statsOne = doChords . stripData
    
    doChords :: [ChordLabel] -> Transitions
    doChords (c1:c2:r) = M.insertWith (+) (c1,c2) 1 (doChords (c2:r))
    doChords _         = ts


instance (Ord a) => Ord (Chord a) where
  compare a b = compare (chordShorthand a) (chordShorthand b) `mappend` 
                compare (chordRoot a) (chordRoot b)

-- Creates a simple chord based on a root and shorthand 
-- TODO: move to harmtrace-base?
shortChord :: Root -> Shorthand -> ChordLabel
shortChord r sh = Chord r sh [] 0 1

-- Pretty-print the transition matrix
{-
printTransitions :: Transitions -> String
printTransitions ts = "\t" ++ 
                      intercalate "\t" [ show c1 | ((c1, c2), _) <- M.toAscList ts
                                                , c1 == c2 ] 
                      ++ "\n" ++ concat
                      [ show c1 ++ "\t"
                        ++ intercalate "\t"
                           [ show p
                           | ((c1', _c2'), p) <- M.toAscList ts, c1 == c1' ] ++ "\n"
                      | ((c1, c2), _) <- M.toAscList ts, c1 == c2 ]          
 -}
 
 
--------------------------------------------------------------------------------
-- Initial probabilities
--------------------------------------------------------------------------------

emptyInit :: InitCount
emptyInit = M.fromList $ zip allChords (repeat 0)    

-- Process all files in the dir
readInitCounts :: [State ChordLabel] -> FilePath -> IO [Prob]
readInitCounts sts fp = do files <- getBBFiles fp
                           ic    <- foldM readSong emptyInit files -- InitCount
                           let l = fromIntegral . length $ files
                           return . map (toProb ic l) $ sts where

  readSong :: InitCount -> (FilePath, Int) -> IO InitCount
  readSong ip (f, _) = 
    do bb <- readFile f >>= return . fst . parseBillboard
       return . M.adjust succ (chord . getData . head . getSong $ bb) $ ip 
    
  toProb :: InitCount -> Float -> State ChordLabel -> Prob 
  toProb ic l s = (fromIntegral $ ic M.! (label s)) / l  
   
 
--------------------------------------------------------------------------------
-- Prepare data for use in ChordTrack.Audio.Viterbi
--------------------------------------------------------------------------------

initViterbiStates :: [ChordLabel] -> [State ChordLabel]
initViterbiStates = zip [0 .. ]

initViterbiTrans :: Transitions -> Matrix Prob
initViterbiTrans = V.fromList . map (V.fromList . map snd) 
                              . splitEvery (length allChords) . M.toList where
  -- we know a map is sorted, just split at every 24 elements
  splitEvery :: Int -> [a] -> [[a]]
  splitEvery _ [] = []  
  splitEvery n l  = let (row, rest) = splitAt n l in row : splitEvery n rest

countsToProb :: Matrix Prob -> Matrix Prob
countsToProb = V.map (replaceZero . rowProb) where

  rowProb :: Vector Prob -> Vector Prob
  rowProb v = let s = sum v in scale (1 / s) v
  
  replaceZero :: Vector Prob -> Vector Prob
  replaceZero = V.map repl where 
    
    repl :: Prob -> Prob
    repl 0.0 = 1.0e-10
    repl p   = p
  
--------------------------------------------------------------------------------
-- Command line interface
--------------------------------------------------------------------------------

main :: IO ()
main = do (path:_) <- getArgs
          ts    <- statsAll path
          -- putStrLn . show . initViterbiStates . sort $ allChords
          let trns   = countsToProb . initViterbiTrans $ ts
              states = initViterbiStates allChords
          initp <- readInitCounts states path
          putStrLn . show  $ states
          putStrLn . show  $ initp
          putStrLn . dispf $ trns
          -- ( [State ChordLabel], [Prob], [[Prob]] )
          encodeFile "transitions.bin" 
             (states, initp, toLists trns)
             
