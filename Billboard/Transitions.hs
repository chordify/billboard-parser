{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main ( main ) where

import qualified Data.Map as M

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep (Chord(..), ChordLabel, toMajMinChord)

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..) )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.List (intercalate)


type Transitions = M.Map (ChordLabel, ChordLabel) Double

-- Get the chords, throw away the rest
stripData :: BillboardData -> [ChordLabel]
stripData = map (toMajMinChord . chord . getData) . getSong

-- Process all files in the dir
statsAll :: FilePath -> IO Transitions
statsAll fp = do files <- getBBFiles fp
                 ts <- foldM doFiles M.empty files
                 -- now show probability instead of counts
                 let nrTs = M.foldr (+) 0 ts
                 return $ M.map (/ nrTs) ts where
  doFiles ts (d,_) = readFile d >>= return . statsOne . fst . parseBillboard where
    statsOne :: BillboardData -> Transitions
    statsOne = doChords . stripData
    
    doChords :: [ChordLabel] -> Transitions
    doChords (c1:c2:r) = M.insertWith (+) (c1,c2) 1 (doChords (c2:r))
    doChords _         = ts

instance (Ord a) => Ord (Chord a) where
  compare a b = compare (chordRoot a) (chordRoot b)

printTransitions :: Transitions -> String
printTransitions ts = intercalate "," [ show c | ((c, _), _) <- M.toAscList ts ] 
                      ++ "\n" ++ concat
                      [ show c1 ++ ","
                        ++ intercalate ","
                           [ show p
                           | ((c1', c2'), p) <- M.toAscList ts, c1 == c1' ] ++ "\n"
                      | ((c1, _), _) <- M.toAscList ts ]
  
main :: IO ()
main = do (path:_) <- getArgs
          ts <- statsAll path
          print ts
          -- putStrLn $ printTransitions ts
