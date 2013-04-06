{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Main ( main ) where

import Prelude 

import qualified Data.Map as M
-- import qualified Data.Vector as V ( fromList, map)
-- import Data.Vector                ( Vector )

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..), reduceTimedBBChords )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Monoid (mappend)
import Data.List (nub, intercalate)
import Data.Binary (encodeFile)
import Text.Printf (printf)

import ChordTrack.Audio.Viterbi
-- import ChordTrack.Audio.VectorNumerics hiding (sum) 

type Transitions = M.Map (ChordLabel, ChordLabel) Int
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
                 $ [ ((c1,c2),0) | c1 <- allChords, c2 <- allChords ]          
                   
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
  toProb ic l s = replZero $ (fromIntegral $ ic M.! (label s)) / l  
   
 
--------------------------------------------------------------------------------
-- Prepare data for use in ChordTrack.Audio.Viterbi
--------------------------------------------------------------------------------

initViterbiStates :: [ChordLabel] -> [State ChordLabel]
initViterbiStates = zip [0 .. ]

initViterbiTrans :: [State ChordLabel] -> Transitions -> [[Prob]]
initViterbiTrans sts trns = map rowProb chrds  where
  
  rowProb :: ChordLabel -> [Prob]
  rowProb fromC = let rc = rowCounts fromC 
                      s  = fromIntegral $ sum rc
                  in  map (\c -> replZero $  (fromIntegral c / s)) rc
                  
  rowCounts :: ChordLabel -> [Int]
  rowCounts fromC = map ((M.!) trns) 
                        (map (fromC,) chrds)

  chrds :: [ChordLabel]
  chrds = snd $ unzip sts

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

replZero :: Prob -> Prob
replZero 0.0 = 1.0e-10
replZero p   = p
  
-- | Displaying a matrix
disp :: (a -> String) -> [[a]] -> String
disp shw = intercalate "\n" . map dispRow

  where dispRow = intercalate " " . foldr (\j js -> shw j : js ) [] 

--------------------------------------------------------------------------------
-- Command line interface
--------------------------------------------------------------------------------

main :: IO ()
main = do (path:_) <- getArgs
          ts    <- statsAll path
          -- print $ allChords
          -- print . initViterbiTrans $ ts
          let states = initViterbiStates allChords
              trns   = initViterbiTrans states ts
          initp <- readInitCounts states path
          print states
          print initp
          putStrLn . disp (printf "\t%.2f") $ trns
          -- putStrLn . dispf $ trns
          -- ( [State ChordLabel], [Prob], [[Prob]] )
          encodeFile "transitions.bin" 
             (states, initp, trns)
             
