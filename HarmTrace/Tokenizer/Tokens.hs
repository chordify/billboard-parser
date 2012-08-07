{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE EmptyDataDecls                 #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE GADTs                          #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module HarmTrace.Tokenizer.Tokens ( ChordToken (..), PieceLabel  (..)
                                  , PieceToken (..), ParseStatus (..)
                                  ) where

import HarmTrace.Base.MusicRep
-- import HarmTrace.HAnTree.Binary
-- import Generics.Instant.TH
-- import Data.Binary
  
--------------------------------------------------------------------------------
-- Tokens for parsing chords
--------------------------------------------------------------------------------

-- merged Chords that will be presented to the parser
data ChordToken = ChordToken { root          :: ScaleDegree
                             , classType     :: ClassType
                             , chords        :: [ChordLabel]
                             , status        :: ParseStatus
                             , chordNumReps  :: Int
                             , dur           :: Int -- duration
                             } 
                             
data ParseStatus = NotParsed | Parsed | Deleted | Inserted
  deriving (Eq, Show)
                             
-- a datatype to store a tokenized chords                              
-- type PieceRelToken = PieceToken ChordDegree
-- type PieceAbsToken = PieceToken ChordLabel
data PieceToken = PieceToken Key [ChordToken]
data PieceLabel = PieceLabel Key [ChordLabel]

--------------------------------------------------------------------------------
-- Instances for Chord Tokens
--------------------------------------------------------------------------------
instance Eq ChordToken where
  (ChordToken sd clss _cs stat _n _d) == (ChordToken sd2 clss2 _cs2 stat2 _n2 _d2) 
    = sd == sd2 && clss == clss2 && stat == stat2

instance Show ChordToken where
  show (ChordToken sd clss _cs Inserted _n _d) = show sd ++ show clss++"[Inserted]"
  show (ChordToken sd clss  cs Deleted  _n _d) = 
    show sd ++ show clss ++ "[Deleted" ++ showChords cs ++ "]"
  show (ChordToken sd clss  cs _ _n d) = show sd ++ show clss ++ '_' : show d 
                                                 ++ showChords cs    
showChords :: Show a => [Chord a] -> String  
showChords = concatMap (\x -> '[' : show x ++ "]") 


--------------------------------------------------------------------------------
-- Binary instances
--------------------------------------------------------------------------------

-- deriveAllL [''ChordToken, ''ParseStatus]

-- instance Binary ChordToken where
  -- put = putDefault
  -- get = getDefault
-- instance Binary ParseStatus where
  -- put = putDefault
  -- get = getDefault