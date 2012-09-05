{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE FlexibleContexts         #-}

module HarmTrace.Tokenizer.Tokenizer ( pChordTokens {-not used yet-}
                                     , pChord
                                     , pSongAbs, toKeyRelTok, pRoot
                                     , pDegrees, pDegree ) where

import HarmTrace.Base.Parsing
import HarmTrace.Base.MusicRep
import HarmTrace.Tokenizer.Tokens

--------------------------------------------------------------------------------
-- Tokenizing: parsing strings into tokens
--------------------------------------------------------------------------------  

-- toplevel parser: parses a string of chord labels into a key relative 
-- representation
pChordTokens :: ListLike s Char => s -> (PieceLabel ,[Error LineColPos])
pChordTokens inp = parseDataWithErrors pSongAbs inp
  
-- Merges duplicate chords and transforms absolute chord labels into key
-- relative tokens that can be parsed by the HarmTrace model 
-- (previously called mergeDups)
toKeyRelTok  :: Key -> [ChordLabel] -> [ChordToken]
toKeyRelTok k (c@(Chord r sh _add _loc d):cs) = toKeyRelTok' k 
      (ChordToken (toScaleDegree k r) (toClassType sh) [c] NotParsed 1 d) cs
toKeyRelTok  _key [] = []
toKeyRelTok' :: Key ->  ChordToken -> [ChordLabel] -> [ChordToken]
toKeyRelTok' _k p [] = [p]
toKeyRelTok' k p@(ChordToken deg clss cs' _stat n d1) (c@(Chord r sh _a _l d2):cs) 
  | deg == deg2 && clss == clss2 = 
      toKeyRelTok' k (ChordToken deg clss (cs' ++ [c]) NotParsed (n+1) (d1+d2)) cs
  | otherwise = p : toKeyRelTok' k (ChordToken deg2 clss2 [c] NotParsed 1 d2) cs
  where clss2 = toClassType sh
        deg2  = toScaleDegree k r

-- Input is a string of whitespace-separated chords, e.g.
-- Bb:9(s11) E:min7 Eb:min7 Ab:7 D:min7 G:7(13) C:maj6(9)
-- First token is the key of the piece
pSongAbs :: Parser PieceLabel -- PieceRelToken -- 
pSongAbs = PieceLabel <$> pKey <* pLineEnd 
                      <*> (setLoc 0 <$> pListSep_ng pLineEnd pChordDur )
                      <*  pList pLineEnd where
  setLoc :: Int -> [Chord a] -> [Chord a]  
  setLoc _  [] = []
  setLoc ix (Chord r c d _ l :cs) = (Chord r c d ix l) : setLoc (ix+1) cs                               

-- parses chords with a duration (separated by a ';')
pChordDur :: Parser ChordLabel
pChordDur = setDur <$> pChord <*> (pSym ';' *> pNaturalRaw) where
  setDur c d = c {duration = d}

pChord :: Parser ChordLabel 
pChord =     pChordLabel 
         <|> (noneLabel    <$ (pString "N" <|> pString "&pause"))
         <|> (unknownLabel <$ (pString "*" <|> pString "X"))

-- For now, I assume there is always a shorthand, and sometimes extra
-- degrees. I guess it might be the case that sometimes there is no shorthand,
-- but then there certainly are degrees. 
pChordLabel :: Parser ChordLabel
pChordLabel = toChord <$> pRoot <* pSym ':'  <*> pMaybe pShorthand
                      -- we ignore optional inversions for now
                      <*> ((pDegrees `opt` []) <* pInversion)
  where -- if there are no degrees and no shorthand 
        toChord :: Root -> Maybe Shorthand -> [Addition] -> ChordLabel
        toChord r Nothing     [] = Chord r None [] 0 1
        toChord r Nothing     d  = case analyseDegTriad d of
                                     MajTriad -> Chord r Maj (remTriadDeg d) 0 1
                                     MinTriad -> Chord r Min (remTriadDeg d) 0 1
                                     AugTriad -> Chord r Aug (remTriadDeg d) 0 1
                                     DimTriad -> Chord r Dim (remTriadDeg d) 0 1
                                     NoTriad  -> Chord r None d 0 1
        toChord r (Just s)    d  = Chord r s d 0 1
        
        -- removes the third and the fifth from a Addtion list
        remTriadDeg :: [Addition] -> [Addition]
        remTriadDeg = filter (\(Add (Note _ i)) -> i /= I3 || i /= I5)

pInversion :: Parser (Maybe (Note Interval))
pInversion = (Just <$> (pSym '/' *> (Note <$> pMaybe pModifier <*> pInterval))
             ) `opt` Nothing
-- parses a musical key description
pKey :: Parser Key        
pKey = f <$> pRoot <* pSym ':' <*> pShorthand
  where f r m | m == Maj = Key r MajMode
              | m == Min = Key r MinMode
              | otherwise = error ("Tokenizer: key must be Major or Minor, "
                          ++ "found: " ++ show m)

pShorthand :: Parser Shorthand
pShorthand =     Maj      <$ pString "maj"
             <|> Min      <$ pString "min"
             <|> Dim      <$ pString "dim"
             <|> Aug      <$ pString "aug"
             <|> Maj7     <$ pString "maj7"
             <|> Min7     <$ pString "min7"
             <|> Sev      <$ pString "7"
             <|> Dim7     <$ pString "dim7"
             <|> HDim7    <$ pString "hdim" <* opt (pSym '7') '7'
             <|> MinMaj7  <$ pString "minmaj7"
             <|> Maj6     <$ pString "maj6"
             <|> Maj6     <$ pString "6"
             <|> Min6     <$ pString "min6"
             <|> Nin      <$ pString "9"
             <|> Maj9     <$ pString "maj9"
             <|> Min9     <$ pString "min9"
             <|> Five     <$ pString "5" 
             <|> Sus2     <$ pString "sus2" 
             <|> Sus4     <$ pString "sus4" 
             -- additional Billboard shorthands
             <|> Min11    <$ pString "min11" 
             <|> Min13    <$ pString "min13" 
             <|> Maj13    <$ pString "maj13" 
             <|> Eleven   <$ pString "11" 
             <|> Thirteen <$ pString "13" 
             <|> None     <$ pString "1" -- no shorthand: used in billboard to 
                                         -- denote a rootnote only
             <?> "Shorthand"

-- We don't produce intervals for a shorthand. This could easily be added,
-- though.
pDegrees :: Parser [Addition]
pDegrees = pPacked (pSym '(') (pSym ')') ( pListSep (pSym ',') pDegree )

-- TODO removing degrees is not implemented 
pDegree :: Parser Addition
pDegree =   (Add   <$>              (Note <$> pMaybe pModifier <*> pInterval))
        <|> (NoAdd <$> (pSym '*' *> (Note <$> pMaybe pModifier <*> pInterval)))
              
pModifier :: Parser Modifier
pModifier =     Sh <$ pSym    's'
            <|> Sh <$ pSym    '#'
            <|> Fl <$ pSym    'b'
            <|> SS <$ pString "ss"
            <|> FF <$ pString "bb" <?> "Modifier"

pInterval :: Parser Interval
pInterval =  ((!!) [minBound..] ) . pred <$> pNaturalRaw

pRoot :: Parser Root
pRoot =     Note Nothing   A  <$ pSym 'A'
        <|> Note Nothing   B  <$ pSym 'B'
        <|> Note Nothing   C  <$ pSym 'C'
        <|> Note Nothing   D  <$ pSym 'D'
        <|> Note Nothing   E  <$ pSym 'E'
        <|> Note Nothing   F  <$ pSym 'F'
        <|> Note Nothing   G  <$ pSym 'G'
        <|> Note (Just Fl) A <$ pString "Ab"
        <|> Note (Just Fl) B <$ pString "Bb"
        <|> Note (Just Fl) C <$ pString "Cb"
        <|> Note (Just Fl) D <$ pString "Db"
        <|> Note (Just Fl) E <$ pString "Eb"
        <|> Note (Just Fl) F <$ pString "Fb"
        <|> Note (Just Fl) G <$ pString "Gb"
        <|> Note (Just Sh) A <$ pString "A#"
        <|> Note (Just Sh) B <$ pString "B#"
        <|> Note (Just Sh) C <$ pString "C#"
        <|> Note (Just Sh) D <$ pString "D#"
        <|> Note (Just Sh) E <$ pString "E#"
        <|> Note (Just Sh) F <$ pString "F#"
        <|> Note (Just Sh) G <$ pString "G#" <?> "Chord root"
