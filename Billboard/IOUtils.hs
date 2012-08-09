{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Billboard.IOUtils
-- Copyright   :  (c) 2012 Universiteit Utrecht
-- License     :  GPL3
--
-- Maintainer  :  W. Bas de Haas <W.B.deHaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A set Billboard specific file and directory utilities
--------------------------------------------------------------------------------

module Billboard.IOUtils (bbdir, getBBFiles, getBBFile)where

import System.Directory
import System.FilePath
import Text.Printf (printf)

-- | Applies a function to all files in a directory
bbdir :: (FilePath -> IO a) ->  FilePath -> IO [a]
bbdir f fp = do dirs <- getDirectoryContents fp
                mapM (\d -> f (fp </> d </> "salami_chords.txt")) 
                     (filter isDir dirs) 

-- | Given the path to the Billboard collection, returns a list with the
-- filepaths and id's of the salami_chords.txt files. (The id is the parent
-- folder name.)
getBBFiles :: FilePath -> IO [(FilePath, Int)]
getBBFiles p = do dirs <- getDirectoryContents p
                  mapM (\d -> return (p </> d </> "salami_chords.txt", read d)) 
                       (filter isDir dirs) 

-- Returns False on ".." and "."
isDir :: String -> Bool
isDir x = x /= ".." && x /= "."
 
-- | Given a base directory pointing to the billboard location and a billboard
-- id, this function returns the path to that particular billboard file. If
-- the file does not exist, an error is thrown.
getBBFile :: FilePath -> Int -> IO (FilePath)
getBBFile billboardLoc nr = 
  do let  fp = billboardLoc </> printf "%04d" nr </> "salami_chords.txt"
     fpExist <- doesFileExist fp
     case fpExist of
       True  -> return fp 
       False -> error ("Error: " ++ printf "%04d" nr 
                  ++ " is not a valid billboard id, or the directory " 
                  ++ billboardLoc ++"does not point to the billboard collection"
                  ++ " Regardless, the file " ++ fp ++ " does not exist" )
