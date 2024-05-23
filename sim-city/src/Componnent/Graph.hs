module Componnent.Graph where

import Componnent.Forme 
import Componnent.Zone

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (minimumBy, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Ord (comparing)
import qualified Data.Set as Set

-- Type pour représenter un graphe 
-- Un graphe est une map de zone et de ses voisins qui sont representés par une map de zoneId et de un couple de la liste 
-- des coordonnées  qu'il a d'adjacence avec la forme de la zone voisine

-- Fonction pour construire le graphe des routes
type Graph = Map ZoneId (Map ZoneId ([Coord], Forme))

buildGraph :: Map ZoneId Forme -> Graph
buildGraph routes = Map.fromListWith Map.union [(zoneId, adjacentZones zoneId forme) | (zoneId, forme) <- Map.toList routes]
  where
    adjacentZones zoneId forme = Map.fromList [(zoneId', (coords, forme')) | (zoneId', forme') <- Map.toList routes, zoneId /= zoneId', Just coords <- [formesAdjacentPoints forme forme']]

-- Fonction pour vérifier la connexité du graphe
isConnected :: Graph -> Bool
isConnected graph = not (null graph) && length visited == length (Map.keys graph)
  where
    startNode = fst $ head $ Map.toList graph
    visited = dfs startNode []
    dfs node visitedNodes
      | node `elem` visitedNodes = visitedNodes
      | otherwise = foldr dfs (node : visitedNodes) (Map.keys (Map.findWithDefault Map.empty node graph))

-- Heuristique pour A* : distance de Manhattan
manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Fonction pour obtenir les voisins d'une coordonnée
voisins :: Coord -> [Coord]
voisins (C x y) = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1)]

-- Fonction pour vérifier si une coordonnée est une route
isRoute :: Map Coord Char -> Coord -> Int -> Bool
isRoute gameMap coord margin =
    let offsets = [ (dx, dy) | dx <- [-margin..margin], dy <- [-margin..margin] ]
        surroundingCoords = [ C (cx coord + dx) (cy coord + dy) | (dx, dy) <- offsets ]
    in any (\c -> Map.lookup c gameMap `elem` [Just '#', Just '^']) surroundingCoords

-- Fonction A* pour trouver le chemin le plus court entre deux points dans une carte
aStar :: Map Coord Char -> Coord -> Coord -> Maybe [Coord]
aStar gameMap start goal = reconstructPath <$> search
  where
    heuristic = manhattanDistance

    search = astar' (Set.singleton start) [(start, 0)] Map.empty (Map.singleton start 0)

    astar' openSet queue cameFrom gScore
      | Set.null openSet = Nothing
      | current == goal = Just cameFrom
      | otherwise = astar' openSet' queue' cameFrom' gScore'
      where
        current = fst $ head queue
        currentScore = fromMaybe (error "current not in gScore") (Map.lookup current gScore)

        validNeighbors = filter (\neighbor -> isRoute gameMap neighbor 100) (voisins current)
        (openSet', queue', cameFrom', gScore') = foldr visit (Set.delete current openSet, tail queue, cameFrom, gScore) validNeighbors

        visit neighbor (openSet, queue, cameFrom, gScore) =
          let tentativeGScore = currentScore + 1
              neighborGScore = Map.findWithDefault maxBound neighbor gScore
          in if tentativeGScore < neighborGScore
             then (Set.insert neighbor openSet,
                   insertSorted (neighbor, tentativeGScore + heuristic neighbor goal) queue,
                   Map.insert neighbor current cameFrom,
                   Map.insert neighbor tentativeGScore gScore)
             else (openSet, queue, cameFrom, gScore)

    reconstructPath cameFrom
      | start == goal = []
      | otherwise = reverse $ go goal []
      where
        go current path
          | current == start = path
          | otherwise = go (fromMaybe (error "cameFrom incomplete") (Map.lookup current cameFrom)) (current : path)

    insertSorted :: (a, Int) -> [(a, Int)] -> [(a, Int)]
    insertSorted x [] = [x]
    insertSorted x ys@(y:ys')
      | snd x <= snd y = x : ys
      | otherwise = y : insertSorted x ys'

-- Fonction pour trouver la prochaine direction
findNext :: Coord -> Coord -> Map Coord Char -> Maybe Coord
findNext start goal gameMap = case aStar gameMap start goal of
  Nothing -> Nothing
  Just path -> if null path then Nothing else Just (last path)