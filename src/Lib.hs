module Lib where

import Data.List
import Control.Monad

type Edge = (Int, Int)
type EdgeList = [Edge]
type EdgeSet = [EdgeList]
type Node = [Int]

someFunc :: IO ()
someFunc = do
    let ansers = search [board 15]
    forM_ ansers print

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

erase :: EdgeSet -> Edge -> EdgeSet
erase set edge = flip map set $ \list ->
    flip filter list $ \other ->
        (fst edge) /= (fst other) &&
        (fst edge) /= (snd other) &&
        (snd edge) /= (snd other) &&
        (snd edge) /= (fst other)

setNode :: Node -> Int -> Edge -> Node
setNode node num edge = flip map (zip node [1..]) $ \t ->
    if snd t == fst edge || snd t == snd edge
        then num
        else fst t

search :: [Board] -> [Node]
search [] = []
search (b:bs) = case b of
    Board 0 node _    -> node : search bs
    Board n node list ->
        search (candidate b) ++ search bs

candidate :: Board -> [Board]
candidate (Board n node set) =
    flip map (set !! (n-1)) $ \edge ->
    let
        set' = erase set edge
        node' = setNode node n edge
    in
        Board (n-1) node' set'






