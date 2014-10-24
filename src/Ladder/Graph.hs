module Ladder.Graph where

import Data.List(intercalate)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Graph.AStar as A

newtype Ladder = Ladder [String]
instance Show Ladder where
  show (Ladder xs) = intercalate " -> " xs

type WordGraph = HM.HashMap String [String]

distance :: String -> String -> Int
distance a b = foldr (\(la,lb) total -> if (la == lb) then total else total+1) 0 (zip a b)

buildGraph :: [String] -> WordGraph
buildGraph ws = foldr (\k m -> HM.insert k (filter (\w -> distance w k == 1) ws) m) HM.empty ws

shortestPath :: WordGraph -> String -> String -> Maybe [String]
shortestPath dict start finish = A.aStar graph (\_ _ -> 1) (\w -> distance w finish) (\w -> w == finish) start
                               where graph = \w -> case HM.lookup w dict of
                                                    Nothing -> S.empty
                                                    Just s -> S.fromList s

