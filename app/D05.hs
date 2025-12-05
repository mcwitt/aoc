module Main where

import AoC.Prelude
import Control.Arrow ((&&&), (>>>))
import Text.ParserCombinators.ReadP

range :: ReadP (Int, Int)
range = (,) <$> readS_to_P reads <* char '-' <*> readS_to_P reads

ranges = range `sepBy1` char '\n'

ingredient :: ReadP Int
ingredient = readS_to_P reads

ingredients = ingredient `sepBy1` char '\n'

parser = (,) <$> ranges <* char '\n' <*> ingredients <* char '\n'

parse = unsafeParse parser

solve1 (ranges, ingredients) =
  length $
    filter (\ingredient -> any (\(a, b) -> a <= ingredient && ingredient <= b) ranges) $
      ingredients

solve2 (ranges, _) = sum $ map (\(a, b) -> b - a + 1) $ foldl' merge [] ranges

merge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
merge [] r = [r]
merge (r : rs) s = case mergeOverlapping r s of
  Nothing -> r : merge rs s
  Just r' -> merge rs r'

mergeOverlapping :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
mergeOverlapping r@(a1, b1) s@(a2, b2)
  | a1 <= a2 = go r s
  | otherwise = go s r
  where
    go (a1, b1) (a2, b2)
      | b1 < a2 = Nothing
      | otherwise = Just (min a1 a2, max b1 b2)

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
