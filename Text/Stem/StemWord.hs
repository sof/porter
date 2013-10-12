--------------------------------------------------------------------
-- |
-- Module    : Text.Stem.StemWord
-- Copyright : (c) Sigbjorn Finne, 2013
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------
module Text.Stem.StemWord where

import Data.Maybe ( isJust )

-- A word/string to be stemmed.
data StemWord
 = StemWord {
      -- The original, un-stemmed word.
     original :: String,
      -- the reversed word, with possibly a suffix removed (if so, 'updated' is True.)
     suffix   :: String,
      -- suffix characters added (for normalization purposes.)
     adds     :: String,
      -- True if 'suffix' has been trimmed.
     updated  :: Bool
   }

wordLength :: StemWord -> Int
wordLength sw = length (adds sw) + length (suffix sw)

lastChar :: StemWord -> Char
lastChar sw =
  case adds sw of
    "" -> head (suffix sw)
    (x:_) -> x

indexChar :: Int -> StemWord -> Maybe Char
indexChar i sw =
  case go i (adds sw) of
    (n1, Nothing) -> snd $ go n1 (suffix sw)
    (_, mb) -> mb
  where
   go n "" = (n, Nothing)
   go 0 (x:_) = (0, Just x)
   go n (_:xs) = go (n - 1) xs

-- | remove N characters from the suffix. If N is longer than
-- the word, the empty word is returned.
dropSuffixChars :: Int -> StemWord -> StemWord
dropSuffixChars i sw = snd $
    -- the word "fugly" springs to mind.. First peel off
    -- the characters appended, followed by the reversed word.
   dropIt (\ s sw1 -> sw1{suffix=s, updated = True}) (suffix sw) $
   dropIt (\ s sw1 -> sw1{adds = s, updated = True}) (adds sw) (i, sw)
  where
    dropIt upd "" (n, sw1) = (n, upd "" sw1)
    dropIt upd xs (0, sw1) = (0, upd xs sw1)
    dropIt upd (_:xs) (n1, sw1) = dropIt upd xs (n1 - 1, sw1)

-- | extend stemmed word with a suffix.
addSuffix :: String -> StemWord -> StemWord
addSuffix "" sw = sw
addSuffix s sw =
  case adds sw of
    "" -> sw{adds = reverse s, updated = True}
    xs -> sw{adds = reverse s ++ xs, updated = True}

-- | @endsWith str word@ checks if @str@ matches the suffix of @word@.
endsWith :: String -> StemWord -> Bool
endsWith "" _ = True
endsWith s sw = isJust (foldr matches (Just (adds sw, suffix sw)) s)
 where
   -- matcher propagates Nothing upon non-match,
   -- and remaining string if a match.
  matches _ Nothing = Nothing
  matches _ (Just ([], [])) = Nothing
  matches ch (Just ([], (x:xs)))
    | ch == x = Just ([], xs)
    | otherwise = Nothing
  matches ch (Just (x:xs, ys))
    | ch == x = Just (xs, ys)
    | otherwise = Nothing

-- | @toStemWord s@ converts a string into a StemWord.
toStemWord :: String -> StemWord
toStemWord s
  = StemWord {
      original = s,
      suffix = reverse s,
      adds = "",
      updated = False
    }

-- | @fromStemWord sw@ takes a possibly stemmed word representation and
-- returns its final stemmed string.
fromStemWord :: StemWord -> String
fromStemWord sw
 | not (updated sw) = original sw
 | null (adds sw) = rsuff
 | otherwise = rsuff ++ reverse (adds sw)
 where
   rsuff = reverse (suffix sw)
