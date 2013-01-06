{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main ( main ) where

import qualified Data.Map as M

import HarmTrace.Base.MusicTime (TimedData, getData)
import HarmTrace.Base.MusicRep

import Billboard.BillboardParser ( parseBillboard)
import Billboard.BillboardData ( BillboardData (..), BBChord (..) )
import Billboard.IOUtils

import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Monoid (mappend)
import Data.List (nub, intercalate)


type Transitions = M.Map (ChordLabel, ChordLabel) Double


allRoots :: [Root]
-- To keep things simple, we generate a lot of notes, and then simplify them
allRoots = nub . map simplifyRoot $
           [ Note m r | m <- [Nothing, Just Sh, Just Fl]
                      , r <- [C,D,E,F,G,A,B] ]

allChords :: [ChordLabel]
allChords = [ Chord r sh [] 0 1 | sh <- [Maj,Min,None], r <- allRoots ]

emptyTransitions :: Transitions
emptyTransitions = M.fromList $
                   [ ((c1,c2),0.0) | c1 <- allChords, c2 <- allChords ]

-- Get the chords, throw away the rest
stripData :: BillboardData -> [ChordLabel]
stripData = filter isBad . map (simplify . chord . getData) . getSong where
  simplify :: ChordLabel -> ChordLabel
  simplify c = toMajMinChord c { chordRoot = simplifyRoot (chordRoot c)
                               , chordAdditions = []
                               , getLoc = 0, duration = 1 }
  
  isBad :: ChordLabel -> Bool
  isBad (Chord (Note _ N) _ _ _ _) = False
  isBad (Chord (Note _ X) _ _ _ _) = False
  isBad _                          = True

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
printTransitions ts = "," ++ 
                      intercalate "," [ show c1 | ((c1, c2), _) <- M.toAscList ts
                                                , c1 == c2 ] 
                      ++ "\n" ++ concat
                      [ show c1 ++ ","
                        ++ intercalate ","
                           [ show p
                           | ((c1', c2'), p) <- M.toAscList ts, c1 == c1' ] ++ "\n"
                      | ((c1, c2), _) <- M.toAscList ts, c1 == c2 ]
  
main :: IO ()
main = do (path:_) <- getArgs
          ts <- statsAll path
          -- putStrLn $ printTransitions ts
          print (M.toAscList ts)
