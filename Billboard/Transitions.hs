{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main ( main ) where

import qualified Data.Map as M
import qualified Data.Vector as V ( length, maximumBy, maxIndexBy, replicate, zipWith, (!), foldr, map, last )
import Data.Vector                ( Vector, generate, fromList, (//))

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..) )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Monoid (mappend)
import Data.List (nub, intercalate)
import Data.Binary (encodeFile)

import ChordTrack.Audio.Viterbi

type Transitions = M.Map (ChordLabel, ChordLabel) Float


allRoots :: [Root]
-- To keep things simple, we generate a lot of notes, and then simplify them
allRoots = nub . map simplifyRoot $
           [ Note m r | m <- [Nothing, Just Sh, Just Fl]
                      , r <- [C,D,E,F,G,A,B] ]

allChords :: [ChordLabel]
allChords = [ Chord r sh [] 0 1 | sh <- [Maj,Min], r <- allRoots ]

emptyTransitions :: Transitions
emptyTransitions = M.fromList $
                   [ ((c1,c2),0.0) | c1 <- allChords, c2 <- allChords ]

-- Get the chords, throw away the rest
stripData :: BillboardData -> [ChordLabel]
stripData = filter isGood . map (simplify . chord . getData) . getSong where
  simplify :: ChordLabel -> ChordLabel
  simplify c = toMajMinChord c { chordRoot = simplifyRoot (chordRoot c)
                               , chordAdditions = []
                               , getLoc = 0, duration = 1 }
  
  isGood :: ChordLabel -> Bool
  isGood (Chord (Note _ N) _    _ _ _) = False
  isGood (Chord (Note _ X) _    _ _ _) = False
  isGood (Chord _          None _ _ _) = False -- for chords without a triad
  isGood _                             = True

-- Process all files in the dir
statsAll :: FilePath -> IO Transitions
statsAll fp = do files <- getBBFiles fp
                 ts <- foldM doFiles emptyTransitions files
                 -- uncomment to show probability instead of counts
                 -- let nrTs = M.foldr (+) 0 ts
                 -- return $ M.map (/ nrTs) ts where
                 return ts where
  doFiles ts (d,_) = readFile d >>= return . statsOne . fst . parseBillboard where
    statsOne :: BillboardData -> Transitions
    statsOne = doChords . stripData
    
    doChords :: [ChordLabel] -> Transitions
    doChords (c1:c2:r) = M.insertWith (+) (c1,c2) 1 (doChords (c2:r))
    doChords _         = ts

instance (Ord a) => Ord (Chord a) where
  compare a b = compare (chordRoot a) (chordRoot b) `mappend`
                compare (chordShorthand a) (chordShorthand b)

-- Pretty-print the transition matrix
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
  

-- toViterbiStateTrans :: Transitions -> ([State ChordLabel], [Trans ChordLabel])
-- toViterbiStateTrans t = (M.elems stateMap, map toTrans . M.toList $ t) where

  -- stateMap ::  M.Map ChordLabel (State ChordLabel)
  -- stateMap = snd . M.mapAccumWithKey acc 0 
                 -- . M.mapKeysMonotonic fst 
                 -- . M.filterWithKey toCMaj $ t

  -- acc :: Int -> ChordLabel -> Double -> (Int, State ChordLabel)
  -- acc i c _ = (succ i, State i c)
    
  -- toCMaj :: (ChordLabel, ChordLabel) -> Double -> Bool
  -- toCMaj (_from, Chord (Note Nothing C) Maj [] 0 1) _ = True
  -- toCMaj  _                                         _ = False

  -- toTrans :: ((ChordLabel, ChordLabel), Double) -> Trans ChordLabel
  -- toTrans ((from, to), p) = Trans ( fromJust $ M.lookup from stateMap
                                  -- , fromJust $ M.lookup to stateMap
                                  -- , double2Float p)

-- | Given a list of 'State''s and 'Trans'itions, creates a square transition 
-- matrix that allows for fast lookup of a transistion of any 'State' to
-- any other 'State'. If there is no transistion between two states a 0 is 
-- stored in the matrix
initTransProb :: forall a. [State a] -> [Trans a] -> Matr Prob
initTransProb sts ts = 
  let len  = length sts
      x = V.replicate len (V.replicate len 0)  -- initial matrix with zeros
      
      upd :: Vect Prob -> State a -> Vect Prob
      upd v s = v // collectState s ts
  
      -- Collects the transistion that leave a state as a list of state indices 
      -- with probabilities
      collectState :: State a -> [Trans a] -> [(Int, Prob)]
      collectState (State from _) = foldr step [] where

        step :: Trans a -> [(Int, Prob)] -> [(Int, Prob)]
        step (Trans (State a _, State to _, p)) rest 
          | from == a = (to, p) : rest -- the state matches the from state
          | otherwise = rest
  
  in V.zipWith upd x (fromList sts)                                  
  
splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []  
splitEvery n l  = let (row, rest) = splitAt n l in row : splitEvery n rest
  
main :: IO ()
main = do (path:_) <- getArgs
          ts <- statsAll path
          mapM_ (putStrLn . show) (splitEvery 24 . M.toList $ts)
          -- putStrLn $ printTransitions ts
          -- encodeFile "transitions.bin" (M.toAscList ts)
