module Main where

import Control.Arrow ((&&&), (>>>))
import Data.List

parse = lines

type Tile = Char

type Grid a = [[a]]

windows :: Int -> [a] -> [[a]]
windows k = foldr (zipWith (:)) (repeat []) . take k . tails

windowsGrid :: Int -> Grid a -> Grid (Grid a)
windowsGrid k = map (map transpose . windows k . transpose) . windows k

pad :: a -> [a] -> [a]
pad empty xs = empty : xs ++ [empty]

padGrid :: a -> Grid a -> Grid a
padGrid empty xss =
  let w = length (head xss)
   in pad (replicate (w + 2) empty) $ map (pad empty) $ xss

accessible :: Grid Tile -> Grid Bool
accessible = (map . map) kernel . windowsGrid 3 . padGrid '.'
  where
    kernel xss@[_, [_, x, _], _] = x == '@' && length (filter ('@' ==) (concat xss)) < 5

solve1 :: Grid Tile -> Int
solve1 = length . filter id . concat . accessible

zipWithGrid :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipWithGrid = zipWith . zipWith

removeAccessible :: Grid Tile -> Grid Tile
removeAccessible xss = zipWithGrid rule xss (accessible xss)
  where
    rule _ True = '.'
    rule prev False = prev

solve2 :: Grid Char -> Int
solve2 xss = countPaper xss - countPaper xss'
  where
    xss' = head . dropWhile ((> 0) . solve1) . iterate removeAccessible $ xss
    countPaper = length . filter ('@' ==) . concat

main :: IO ()
main = interact (parse >>> (solve1 &&& solve2) >>> show)
