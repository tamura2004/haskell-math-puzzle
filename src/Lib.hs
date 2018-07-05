module Lib where

import System.Random
import Data.List

type Edge = (Int, Int)
type EdgeList = [Edge]
type EdgeSet = [EdgeList]
type Node = [Int]

someFunc :: IO ()
someFunc = undefined

edge :: Int -> Int -> Edge
edge width head = (head, head + width + 1)

edgeList :: Int -> Int -> EdgeList
edgeList tail width
    | width + 1 > tail = []
    | otherwise = take (tail - width -1) $ map (edge width) [1..]

edgeSet :: Int -> EdgeSet
edgeSet n =
    let m = n * 2
    in map (edgeList m) [1..n]

node :: Int -> [Int]
node n = take (n * 2) $ repeat 0

data Board = Board Int Node EdgeSet deriving Show

board :: Int -> Board
board n = Board n (node n) (edgeSet n)

candidate :: EdgeSet -> Edge -> EdgeSet
candidate set edge = flip map set $ \list ->
    flip filter list $ \other ->
        (fst edge) /= (fst other) &&
        (fst edge) /= (snd other) &&
        (snd edge) /= (snd other) &&
        (snd edge) /= (fst other)

set :: Node -> Int -> Edge -> Node
set node num edge = flip map (zip node [1..]) $ \t ->
    if snd t == fst edge || snd t == snd edge
        then num
        else fst t
