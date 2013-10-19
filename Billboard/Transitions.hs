{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Main ( main ) where

import Prelude 

import qualified Data.Map.Strict as M
import HarmTrace.Base.Time       (Timed, getData)
import HarmTrace.Base.Chord

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData   ( BillboardData (..), BBChord (..) )
import Billboard.IOUtils

import System.Environment        ( getArgs )
import Control.Monad             ( foldM )
import Data.List                 ( intercalate )
import Data.Binary               ( encodeFile )
import Text.Printf               ( printf )

import ChordTrack.Audio.Viterbi

type Transitions = M.Map (ChordLabel, ChordLabel) Int
type InitCount   = M.Map ChordLabel Int

allChords :: [ChordLabel]
allChords = NoChord 
          : [ shortChord (pcToRoot r) sh | sh <- [Maj,Min], r <- [0 .. 11] ]

emptyTransitions :: Transitions
emptyTransitions = M.fromList 
                 $ [ ((c1,c2),0) | c1 <- allChords, c2 <- allChords ]          
                   
-- Get the chords, throw away the rest
stripData :: BillboardData -> [ChordLabel]
stripData = map ( ignorePitchSpelling . discardBass 
                . toMajMinChord . chord . getData) . getSong

-- Process all files in the dir
statsAll :: FilePath -> IO Transitions
statsAll fp = bbFold doFiles emptyTransitions fp where
  
  doFiles :: Transitions -> FilePath -> IO Transitions
  doFiles ts d = do putStrLn d
                    readFile d >>= return . statsOne . fst . parseBillboard 
    where
      statsOne :: BillboardData -> Transitions
      statsOne = doChords . stripData
      
      doChords :: [ChordLabel] -> Transitions
      doChords (   UndefChord:t) = doChords t
      doChords (_c:UndefChord:t) = doChords t
      doChords (c1:c2:r)         = M.insertWith (+) (c1,c2) 1 (doChords (c2:r))
      doChords _                 = ts

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

-- | a 'State' is tuple of a label and an index
initViterbiStates :: [ChordLabel] -> [State ChordLabel]
initViterbiStates = zip [0 .. ]

initViterbiTrans :: [State ChordLabel] -> Transitions -> [[Prob]]
initViterbiTrans sts trns = map rowProb chrds  where
  
  rowProb :: ChordLabel -> [Prob]
  rowProb fromC = let rc = rowCounts fromC 
                      s  = fromIntegral $ sum rc
                  in  map (\c -> replZero $  (fromIntegral c / s)) rc
                  
  rowCounts :: ChordLabel -> [Int]
  rowCounts fromC = map ((M.!) trns . (fromC,)) chrds

  chrds :: [ChordLabel]
  chrds = snd $ unzip sts

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

replZero :: Prob -> Prob
replZero 0.0 = 1.0e-32
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
          putStrLn ("Chords: " ++ show allChords)
          print ts
          putStrLn ("SizeL " ++ show (M.size ts))
          let states = initViterbiStates allChords
              trns   = initViterbiTrans states ts
          initp <- readInitCounts states path
          putStrLn ("States: " ++ show states)
          putStrLn ("Initial Probabilities: " ++ show initp)
          putStrLn . disp (printf "\t%.4f") $ trns
          -- ( [State ChordLabel], [Prob], [[Prob]] )
          encodeFile "transitions.bin" 
             (states, initp, trns)
             
