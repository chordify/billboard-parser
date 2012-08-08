{-# OPTIONS_GHC -Wall #-}
module Main where

import Billboard.BillboardData ( BBChord (..), getBBChords
                               , BillboardData(..), getTitle, showInMIREXFormat)
import Billboard.BillboardParser ( parseBillboard )
import Billboard.Annotation (isUnknown, Annotation )
import Billboard.Tests (mainTest, dirTest)

-- harmtrace imports
import HarmTrace.Audio.ChordTypes (TimedData (..), getData)

-- other libraries
import System.Console.ParseArgs
import System.Directory
import System.FilePath
import Control.Monad (when)
import Text.Printf (printf)

data ReppatArgs = InputFilepath | InputDirFilepath | InputID 
                  deriving (Eq, Ord, Show)

myArgs :: [Arg ReppatArgs]
myArgs = [
           Arg { argIndex = InputFilepath,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input file (containing chord information)"
               }
         , Arg { argIndex = InputDirFilepath,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input base directory (containing the billboard dataset)"
               }
         , Arg { argIndex = InputID,
                 argAbbr  = Just 'i',
                 argName  = Just "id",
                 argData  = argDataOptional "filepath" ArgtypeInt,
                 argDesc  = (  "Input identifier (0-1000). Can only be used in "
                            ++ "combination with a base directory" )
               }
         ]

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          let inFile = getArg arg InputFilepath
              inDir  = getArg arg InputDirFilepath
              bbid   = getArg arg InputID
          case (inFile, inDir, bbid) of
            (Just f , Nothing, Nothing) ->  
               readFile f >>= putStrLn . showInMIREXFormat .fst . parseBillboard
            (Nothing, Just d , Nothing) -> parseCheck d
            (Nothing, Just d , Just i ) -> getBBFile d i >>= oneFile
            _ -> usageError arg "Invalid arguments"
            

--------------------------------------------------------------------------------
-- Testing Salamiparser
-------------------------------------------------------------------------------- 

-- test :: Int -> IO ()
-- test nr = getBBFile nr >>= mainTest

-- testAllBB :: IO ()
-- testAllBB = do dirs <- getDirectoryContents billboardLoc
               -- dirTest . map f $ filter isDir dirs where
                 -- f d = billboardLoc </> d </> "salami_chords.txt"

-- parseBB :: FilePath -> Int -> IO ()
-- parseBB fp nr = getBBFile fp nr >>= oneFile

getBBFile :: FilePath -> Int -> IO (FilePath)
getBBFile billboardLoc nr = 
  do let  fp = billboardLoc </> printf "%04d" nr </> "salami_chords.txt"
     fpExist <- doesFileExist fp
     if  fpExist then return fp
         else do print ("Error: " ++ printf "%04d" nr 
                                  ++ " is not a valid billboard id, the file "
                                  ++ fp ++ " does not exist" )
                 return ("")

oneFile :: FilePath -> IO ()
oneFile fp = do inp <- readFile fp
                let (bbd, err) = parseBillboard inp
                -- putStrLn (getArtist bbd ++ " - " ++ getTitle bbd)
                -- mapM_ print $ getSong bbd
                printBillboard bbd
                mapM_ print err


printBillboard :: BillboardData -> IO()
printBillboard bbd = do putStrLn (getArtist bbd ++ ": " ++ getTitle bbd)
                        putStr $ concatMap (\x -> ' ' : show x) (getBBChords bbd)
                        putStr " |\n" 

-- parses a directory of Billboard songs
parseCheck :: FilePath -> IO ()
parseCheck = bbdir oneSliceSalami where
    
    oneSliceSalami :: FilePath -> IO ([TimedData BBChord])
    oneSliceSalami d = do inp <- readFile (d ++ "\\salami_chords.txt")
                          let (bbd, err) = parseBillboard inp
                              s          = getSong bbd
                          putStrLn (d ++ "\\salami_chords.txt, beats: " 
                                      ++ show (length s) ++ ", errors: " 
                                      ++ show (length err))
                          when (not $ null err) (mapM_ print err)
                          return s


    findUnknowns :: [TimedData BBChord] -> [Annotation]
    findUnknowns = 
      filter isUnknown . concatMap annotations . map getData 
    

bbdir :: (FilePath -> IO a) ->  FilePath -> IO ()
bbdir f fp = do dirs <- getDirectoryContents fp
                mapM_ (\x -> f (fp </> x)) (filter isDir dirs)

isDir :: String -> Bool
isDir x = x /= ".." && x /= "."
    
printMirex :: FilePath -> Int -> IO ()
printMirex fp nr =   getBBFile fp nr >>= readFile 
                 >>= putStrLn . showInMIREXFormat . fst . parseBillboard

              
