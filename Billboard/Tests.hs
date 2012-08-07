module Billboard.Tests (mainTest, dirTest) where

import Test.HUnit
import Control.Monad (void)

import HarmTrace.Audio.ChordTypes (TimedData, getData)

import Billboard.BillboardParser (parseBillboard)
import Billboard.BillboardData (BillboardData (..), BBChord (..))

applyTestToList :: (String -> Test) -> [String] -> Test
applyTestToList t fps = TestList (map t fps)

-- oneChordTimedList :: String -> Test
-- oneChordTimedList = 
  -- TestCase . assertBool "More then one chord in chord list"
           -- . and . map oneChord . getSong . fst . parseBillboard 
    -- where oneChord :: TimedData BBChord -> Bool
          -- oneChord = allSame . getData
          
-- oneChordTimedListVerbose :: String -> Test
-- oneChordTimedListVerbose = 
  -- TestList . map oneChord . getSong . fst . parseBillboard 
    -- where oneChord :: TimedData BBChord -> Test
          -- oneChord c = TestCase $ assertBool 
            -- (show c ++ " contains more then one chord in chord list" )
            -- (allSame $ getData c)
            
allSame :: [BBChord] -> Bool  
allSame (c:cs) = null $ dropWhile ((== chord c) . chord) cs
        
mainTest :: FilePath -> IO ()
mainTest fp = return () -- readFile fp >>= void . runTestTT . oneChordTimedListVerbose

dirTest :: [FilePath] -> IO ()
dirTest fps = return ()
              -- do cs <- mapM readFile fps 
                 -- void . runTestTT $ applyTestToList oneChordTimedList cs
  -- f fp = readFile fp >>= void . runTest . oneChordTimedListVerbose
