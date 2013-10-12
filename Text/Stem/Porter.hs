--------------------------------------------------------------------
-- |
-- Module    : Text.Stem.Porter
-- Copyright : (c) Sigbjorn Finne, 2013
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------
module Text.Stem.Porter where

import Data.Maybe
import Control.Monad (mplus)

--
-- To ease working with the suffix of a word, let's first add an
-- abstraction tailored for the job at hand. It allows the suffix to
-- be readily inspected, and characters dropped and added to it.
--
import Text.Stem.StemWord

--
-- With that in place, we can now start stemming. We will use
-- Porter's original algorithm.
--
-- Keep a close correspondence to the algorithmic formulation
-- used by Porter's paper. And by
--
--   http://snowball.tartarus.org/algorithms/porter/stemmer.html

-- But, the main stemming function first:

stem :: String -> String
stem "" = ""
stem [x] = [x]
stem [x, y] = [x, y]
stem s = fromStemWord $ foldl (\ acc f -> f acc) sw steps
 where
  sw = toStemWord s
  steps = [step1a, step1b, step1c, step2, step3, step4, step5a, step5b]

step1a :: StemWord -> StemWord
step1a sw = findMatch matches sw
 where
  matches = map(\ (a,b) -> (a, const True, b)) $
     [ ("sses", "ss"),
       ("ies", "i"),
       ("ss",  "ss"),
       ("s", "") ]

step1b :: StemWord -> StemWord
step1b sw
 | endsWith "eed" sw =
       -- try reducing to "ee".
     if countVCs (dropSuffixChars 3 sw) > 0 then
        dropSuffixChars 1 sw
     else
        sw
 | otherwise =
    case mbFindMatch (map (\ (a, b) -> (a, hasVowel, b)) $
                          [ ("ed", ""),
                            ("ing", "")]) sw of
      Nothing -> sw
      Just sw1 ->
          -- "ed"/"ing" suffixes removed; tidy up the result.
        case mbFindMatch (map (\ (x, y) -> (x, const True, y)) $
                              [ ("at", "ate"),
                                ("bl", "ble"),
                                ("iz", "ize")]) sw1 of
          Just sw2 -> sw2
          Nothing
           | doubleConsonant sw1 && isConsonant sw1 0 && not (lastChar sw1 `elem` "lsz") ->
             dropSuffixChars 1 sw1
           | countVCs sw1 == 1 && isCVC sw1 -> addSuffix "e" sw1
           | otherwise -> sw1

step1c :: StemWord -> StemWord
step1c sw
 | endsWith "y" sw && hasVowel sw1 = addSuffix "i" sw1
 | otherwise = sw
 where
  sw1 = dropSuffixChars 1 sw

step2 :: StemWord -> StemWord
step2 sw = findMatch matches sw
 where
  matches = map (\ (a, b) -> (a, \ w -> countVCs w > 0, b)) step2_contractions

step3 :: StemWord -> StemWord
step3 sw = findMatch matches sw
 where
  matches = map (\ (a, b) -> (a, \ w -> countVCs w > 0, b)) step3_contractions

step4 :: StemWord -> StemWord
step4 sw = findMatch matches sw
 where
  matches = map (\ (a, b, c) -> (a, fromMaybe (\ w -> countVCs w > 1) b, c)) step4_contractions

step5a :: StemWord -> StemWord
step5a sw
 | endsWith "e" sw && count > 1 = sw1
 | endsWith "e" sw && count == 1 && not (isCVC sw1) = sw1
 | otherwise = sw
 where
   sw1 = dropSuffixChars 1 sw
   count = countVCs sw1

step5b :: StemWord -> StemWord
step5b sw
 | countVCs sw > 1 && lastChar sw == 'l' && doubleConsonant sw = dropSuffixChars 1 sw
 | otherwise = sw

mbFindMatch :: [(String, StemWord -> Bool, String)] -> StemWord -> Maybe StemWord
mbFindMatch [] _ = Nothing
mbFindMatch ((s, predic, sub):xs) sw
 | not (endsWith s sw) = mbFindMatch xs sw
 | predic sw1 = Just (addSuffix sub sw1)
 | otherwise = Nothing
 where
  sw1 = dropSuffixChars (length s) sw

findMatch :: [(String, StemWord -> Bool, String)] -> StemWord -> StemWord
findMatch ls sw = fromJust (mbFindMatch ls sw `mplus` (Just sw))

isConsonant :: StemWord -> Int -> Bool
isConsonant sw i
 | not (isJust mb) = False
 | ch `elem` "aeiou" = False
 | ch == 'y' = not (isConsonant sw (i + 1))
 | otherwise = True
 where
   mb = indexChar i sw
   (Just ch) = mb

isVowel :: StemWord -> Int -> Bool
isVowel sw i = not (isConsonant sw i)

hasVowel :: StemWord -> Bool
hasVowel sw = or [ isVowel sw i | i <- [0.. (wordLength sw - 1)] ]

allConstants :: StemWord -> Bool
allConstants sw = and [ isConsonant sw i | i <- [0.. (wordLength sw - 1)] ]

doubleConsonant :: StemWord -> Bool
doubleConsonant sw = isConsonant sw 0 && Just (lastChar sw) == indexChar 1 sw

countVCs :: StemWord -> Int
countVCs sw = countEm 0 (dropWhile not vcs)
 where
    vcs = [ isConsonant sw i | i <- [0.. (wordLength sw - 1)] ]

    countEm n [] = n
    countEm n xs =
      case dropWhile id xs of
        [] -> n
        xs1 -> countEm (n + 1) (dropWhile not xs1)

isCVC :: StemWord -> Bool
isCVC sw = and [ wordLength sw > 2,
                 isConsonant sw 0,
                 isVowel sw 1,
                 isConsonant sw 2,
                 not (lastChar sw `elem` "wxy"),
                 allConstants (dropSuffixChars 3 sw)]

step2_contractions :: [(String, String)]
step2_contractions = [
    ("ational", "ate"),
    ("tional", "tion"),
    ("enci", "ence"),
    ("anci", "ance"),
    ("izer", "ize"),
    ("bli", "ble"),
    ("alli", "al"),
    ("entli", "ent"),
    ("eli", "e"),
    ("ousli", "ous"),
    ("ization", "ize"),
    ("ation", "ate"),
    ("ator", "ate"),
    ("alism", "al"),
    ("iveness", "ive"),
    ("fulness", "ful"),
    ("ousness", "ous"),
    ("aliti", "al"),
    ("iviti", "ive"),
    ("biliti", "ble"),
    ("logi", "log")]

step3_contractions :: [(String, String)]
step3_contractions = [
    ("icate", "ic"),
    ("ative", ""),
    ("alize", "al"),
    ("iciti", "ic"),
    ("ical", "ic"),
    ("ful", ""),
    ("ness", "")]

step4_contractions :: [(String, Maybe (StemWord -> Bool), String)]
step4_contractions = [
    ("al", Nothing, ""),
    ("ance", Nothing, ""),
    ("ence", Nothing, ""),
    ("er", Nothing, ""),
    ("ic", Nothing, ""),
    ("able", Nothing, ""),
    ("ible", Nothing, ""),
    ("ant", Nothing, ""),
    ("ement", Nothing, ""),
    ("ment", Nothing, ""),
    ("ent", Nothing, ""),
    ("ion", Just (\ w -> countVCs w > 1 && lastChar w `elem` "st"), ""),
    ("ou", Nothing, ""),
    ("ism", Nothing, ""),
    ("ate", Nothing, ""),
    ("iti", Nothing, ""),
    ("ous", Nothing, ""),
    ("ive", Nothing, ""),
    ("ize", Nothing, "")]
