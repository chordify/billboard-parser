{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <W.B.deHaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: The Commandline interface for parsing Billboard data. See:
-- John Ashley Burgoyne, Jonathan Wild, Ichiro Fujinaga, 
-- /An Expert Ground-Truth Set for Audio Chord Recognition and Music Analysis/,
-- In: Proceedings of International Conference on Music Information Retrieval,
-- 2011. (<http://ismir2011.ismir.net/papers/OS8-1.pdf>) 
--------------------------------------------------------------------------------

module Main (main) where

import Billboard.BillboardData ( BBChord (..), getBBChords
                               , BillboardData(..), getTitle, showInMIREXFormat)
import Billboard.BillboardParser ( parseBillboard )
import Billboard.Tests (mainTestFile, mainTestDir)

-- harmtrace imports
import HarmTrace.Audio.ChordTypes (TimedData (..))

-- other libraries
import System.Console.ParseArgs
import System.Directory
import System.FilePath
import Control.Monad (when, void)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Contants
--------------------------------------------------------------------------------

outputFileName :: FilePath
outputFileName = "mirex_chords.txt"

--------------------------------------------------------------------------------
-- Commandline argument parsing
--------------------------------------------------------------------------------
data ReppatArgs = InputFilepath | InputDirFilepath | InputID | ModeArg
                  deriving (Eq, Ord, Show)

myArgs :: [Arg ReppatArgs]
myArgs = [
           Arg { argIndex = ModeArg,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "(parse|mirex)" ArgtypeString,
                 argDesc  = "The operation mode (parse|mirex)"
               }
         , Arg { argIndex = InputFilepath,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input file containing chord information"
               }
         , Arg { argIndex = InputDirFilepath,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input base directory path to the billboard dataset"
               }
         , Arg { argIndex = InputID,
                 argAbbr  = Just 'i',
                 argName  = Just "id",
                 argData  = argDataOptional "filepath" ArgtypeInt,
                 argDesc  = (  "Input identifier (0-1000). Can only be used in "
                            ++ "combination with a base directory" )
               }
         ]

-- representing the mode of operation
data Mode = Mirex | Parse deriving (Eq)

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          -- check whether we have a usable mode
          let mode   = case (getRequiredArg arg ModeArg) of
                         "mirex" -> Mirex
                         "parse" -> Parse
                         m       -> usageError arg ("unrecognised mode: " ++ m)
              -- get filepaths           
              inFile = getArg arg InputFilepath
              inDir  = getArg arg InputDirFilepath
              bbid   = getArg arg InputID
              
          -- the input is either a file (Left) or a directory (Right)
          input  <-  case (inFile, inDir, bbid) of
                        -- we have two ways of identifying a file: by filename
                        (Just f , Nothing, Nothing) -> return (Left f)
                        -- or by basepath and id
                        (Nothing, Just d , Just i ) -> getBBFile arg d i 
                                                       >>= return . Left
                        (Nothing, Just d , Nothing) -> return (Right d)
                        _ -> usageError arg "Invalid filepaths" 
          
          -- do the parsing magic
          case (mode, input) of
            (Mirex, Left  f) -> mirexFile f
            (Mirex, Right d) -> void (mirexDir d)
            (Parse, Left  f) -> parseFile f
            (Parse, Right d) -> parseDir d

            

--------------------------------------------------------------------------------
-- Parsing Billboard data verbosely
-------------------------------------------------------------------------------- 

-- test :: Int -> IO ()
-- test nr = getBBFile nr >>= mainTest

-- testAllBB :: IO ()
-- testAllBB = do dirs <- getDirectoryContents billboardLoc
               -- dirTest . map f $ filter isDir dirs where
                 -- f d = billboardLoc </> d </> "salami_chords.txt"

-- parseBB :: FilePath -> Int -> IO ()
-- parseBB fp nr = getBBFile fp nr >>= oneFile

parseFile :: FilePath -> IO ()
parseFile fp = do inp <- readFile fp
                  let (bbd, err) = parseBillboard inp
                  printBillboard bbd
                  mapM_ print err


printBillboard :: BillboardData -> IO()
printBillboard bbd = 
  do putStrLn (getArtist bbd ++ ": " ++ getTitle bbd)
     putStr $ concatMap (\x -> ' ' : show x) (getBBChords bbd)
     putStr " |\n" 

-- parses a directory of Billboard songs
parseDir :: FilePath -> IO ()
parseDir = void . bbdir oneSliceSalami where
    -- parses a billboard file and presents the user with condenced output
    -- If parsing errors are encountered, they are printed
    oneSliceSalami :: FilePath -> IO ([TimedData BBChord])
    oneSliceSalami d = 
      do inp <- readFile (d </> "salami_chords.txt")
         let (bbd, err) = parseBillboard inp
             s          = getSong bbd
         putStrLn (getArtist bbd ++ ": " ++ getTitle bbd)
         putStrLn (d </> "salami_chords.txt, beats: " ++ show (length s) 
                  ++ ", errors: "                     ++ show (length err))
         when (not $ null err) (mapM_ print err)
         return s

--------------------------------------------------------------------------------
-- Converting the Billboard format to MIREX format
--------------------------------------------------------------------------------

-- Reads a file and prints the chords in mirex format
mirexFile :: FilePath -> IO ()
mirexFile f = readFile f >>= putStrLn . showInMIREXFormat . fst . parseBillboard

-- Reads a directory an writes a file with the chords in mirex format in the 
-- folder containing also the original file
mirexDir :: FilePath -> IO [String]
mirexDir = bbdir toMirex where

  -- read, parses, and writes one mirex chord file
  toMirex :: FilePath -> IO (String)
  toMirex d = 
    do inp <- readFile (d </> "salami_chords.txt")
       let (bbd, err) = parseBillboard inp
           s          = showInMIREXFormat bbd
       when (not $ null err) 
            (error ("there were errors in file: " ++ d </> "salami_chords.txt"))
       writeFile (d </> outputFileName) s
       putStrLn ("written file: " ++ d </> outputFileName)
       return s
     
--------------------------------------------------------------------------------
-- Some (Billboard) file utilities
--------------------------------------------------------------------------------
              
-- | Applies a function to all files in a directory
bbdir :: (FilePath -> IO a) ->  FilePath -> IO [a]
bbdir f fp = do dirs <- getDirectoryContents fp
                mapM (f . (fp </> )) (filter isDir dirs) where

  isDir :: String -> Bool
  isDir x = x /= ".." && x /= "."
  
-- | Given a base directory pointing to the billboard location and a billboard
-- id, this function returns the path to that particular billboard file. If
-- the file does not exist, a usage error is thrown.
getBBFile :: Ord a => Args a -> FilePath -> Int -> IO (FilePath)
getBBFile arg billboardLoc nr = 
  do let  fp = billboardLoc </> printf "%04d" nr </> "salami_chords.txt"
     fpExist <- doesFileExist fp
     case fpExist of
       True  -> return fp 
       False -> usageError arg ("Error: " ++ printf "%04d" nr 
                  ++ " is not a valid billboard id, or the directory " 
                  ++ billboardLoc ++"does not point to the billboard collection"
                  ++ " Regardless, the file " ++ fp ++ " does not exist" )

