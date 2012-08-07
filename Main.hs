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
import Text.Printf (printf)

data ReppatArgs = Filepath deriving (Eq, Ord, Show)

myArgs :: [Arg ReppatArgs]
myArgs = [
          Arg { argIndex = Filepath,
                argAbbr  = Just 'f',
                argName  = Just "file",
                argData  = argDataRequired "filepath" ArgtypeString,
                argDesc  = "Input file (containing chord information)"
              }
         ]

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          readFile (getRequiredArg arg Filepath) >>= 
             putStrLn . showInMIREXFormat . fst . parseBillboard

--------------------------------------------------------------------------------
-- Testing Salamiparser
-------------------------------------------------------------------------------- 

test :: Int -> IO ()
test nr = getBBFile nr >>= mainTest

testAllBB :: IO ()
testAllBB = do dirs <- getDirectoryContents billboardLoc
               dirTest . map f $ filter isDir dirs where
                 f d = billboardLoc </> d </> "salami_chords.txt"

billboardLoc :: String
billboardLoc = "D:\\DATA\\billboard\\Annotations"


getBBFile :: Int -> IO (FilePath)
getBBFile nr = 
  do let  fp = billboardLoc </> printf "%04d" nr </> "salami_chords.txt"
     fpExist <- doesFileExist fp
     if  fpExist then return fp
         else do print ("Error: " ++ printf "%04d" nr 
                                  ++ " is not a valid billboard id, the file "
                                  ++ fp ++ " does not exist" )
                 return ("")

parseBB :: Int -> IO ()
parseBB nr = getBBFile nr >>= oneFile

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

parseCheck :: FilePath -> IO ()
parseCheck = bbdir oneSliceSalami where
    
    oneSliceSalami :: FilePath -> IO ([TimedData BBChord])
    oneSliceSalami d = do inp <- readFile (d ++ "\\salami_chords.txt")
                          let (bbd, err) = parseBillboard inp
                              s          = getSong bbd
                          putStr (d ++ "\\salami_chords.txt, chords: " 
                                    ++ show (length s) ++ ", beats: " )
                                     -- ++ show (sum (map (length . getData s)))
                          if not $ null err 
                            then do putStrLn (", errors: " ++ show (length err)) 
                                    mapM_ print (findUnknowns s)
                                    return s
                            else do putStrLn "" -- linebreak
                                    return s

    findUnknowns :: [TimedData BBChord] -> [Annotation]
    findUnknowns = 
      filter isUnknown . concatMap annotations . map getData 
    
-- parses a directory of Billboard songs
bbdir :: (FilePath -> IO a) ->  FilePath -> IO ()
bbdir f fp = do dirs <- getDirectoryContents fp
                mapM_ (\x -> f (fp </> x)) (filter isDir dirs)

isDir :: String -> Bool
isDir x = x /= ".." && x /= "."
    
printMirex :: Int -> IO ()
printMirex nr =   getBBFile nr >>= readFile 
              >>= putStrLn . showInMIREXFormat . fst . parseBillboard

              
