module Ladder.Graph where

import Data.List(intercalate)
import qualified Data.Set as S
import qualified Data.Graph.AStar as A

newtype Ladder = Ladder [String]
instance Show Ladder where
  show (Ladder xs) = intercalate " -> " xs

type WordGraph = (S.Set String, [String])

distance :: String -> String -> Int
distance a b = foldr (\(la,lb) total -> if (la == lb) then total else total+1) 0 (zip a b)

buildGraph :: [String] -> WordGraph
buildGraph ws = (S.fromList ws, ws)

adjacent :: WordGraph -> String -> S.Set String
adjacent graph node = if (S.member node (fst graph))
                      then S.fromList $ filter (\w -> distance w node == 1) (snd graph)
                      else S.empty

shortestPath :: WordGraph -> String -> String -> Maybe [String]
shortestPath graph start finish =
  A.aStar (adjacent graph) (\_ _ -> 1) (\w -> distance w finish) (\w -> w == finish) start

