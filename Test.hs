module Main(main) where

import Text.Stem.Porter
import Data.Maybe ( catMaybes )
import Control.Monad ( mapM_ )

check :: [String] -> Maybe (String, String, String)
check (a:b:_)
 | s /= b = Just (a, s, b)
 where
  s = stem a
check _ = Nothing


main :: IO ()
main = do
 ls <- readFile "tests/tests.json"
 case reads ls of
   [] -> putStrLn "No parse"
   ((x,_):_) -> do
     let fails = catMaybes (map check x)
     mapM_ print fails
