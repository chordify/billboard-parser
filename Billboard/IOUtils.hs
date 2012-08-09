module Billboard.IOUtils where

import System.Directory
import System.FilePath
import Text.Printf (printf)

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
