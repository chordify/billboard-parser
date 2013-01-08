{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main ( main ) where

import qualified Data.Map as M
import qualified Data.Vector as V ( length, maximumBy, maxIndexBy, replicate, zipWith, (!), foldr, map, last, mapM_)
import Data.Vector                ( Vector, generate, fromList, (//))

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..) )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Monoid (mappend)
import Data.List (nub, intercalate, sort)
import Data.Binary (encodeFile)

import ChordTrack.Audio.Viterbi
import ChordTrack.Audio.VectorNumerics

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
  compare a b = compare (chordShorthand a) (chordShorthand b) `mappend` 
                compare (chordRoot a) (chordRoot b)
                

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

initViterbiStates :: [ChordLabel] -> [State ChordLabel]
initViterbiStates = zipWith State [0 .. ]

initViterbiTrans :: Transitions -> Matr Float
initViterbiTrans = fromList . map (fromList . map snd) 
                            . splitEvery 24 . M.toList where

                            
                            
  splitEvery :: Int -> [a] -> [[a]]
  splitEvery n [] = []  
  splitEvery n l  = let (row, rest) = splitAt n l in row : splitEvery n rest
  
main :: IO ()
main = do (path:_) <- getArgs
          ts <- statsAll path
          -- putStrLn . show . initViterbiStates . sort $ allChords
          let trans = initViterbiTrans ts
          V.mapM_ (putStrLn . show) trans
          encodeFile "transitions.bin" 
             (toLists trans, initViterbiStates allChords)
             
