{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Ville where
import Forme
import Citoyen
import Zone
import Data.Map (Map)
import qualified Data.Map as Map

-- État de la ville
data Ville = Ville {
    viZones :: Map ZoneId Zone,
    viCit :: Map CitId Citoyen
}

-- Collision entre formes
collision :: Forme -> Forme -> Bool
collision  = collision_approx -- À redéfinir selon les regles precise

-- Propriété de non-collision entre zones
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision (Ville zs _) = Map.foldrWithKey aux True zs
  where
    aux _ _ False = False
    aux key z acc = acc && Map.foldr (\z' acc' -> acc' && not (collision (zoneForme z) (zoneForme z'))) True (Map.delete key zs)

-- Propriété que chaque zone est adjacente à une route
prop_ville_routesAdj :: Ville -> Bool
prop_ville_routesAdj (Ville zones _) = Map.foldrWithKey (\_ zone acc -> acc && isAdjacentToRoad zone zones) True zones
  where
    isAdjacentToRoad :: Zone -> Map ZoneId Zone -> Bool
    isAdjacentToRoad zone zonesMap = case zone of
        Route _ -> True  -- Les routes sont toujours ok
        _ -> any (zoneAdjacent zone) (Map.elems zonesMap)

    zoneAdjacent :: Zone -> Zone -> Bool
    zoneAdjacent z1 (Route f2) = adjacentes (zoneForme z1) f2
    zoneAdjacent _ _ = False


-- propriété que chaque batiment a une entrée adjacente à une route et a la zone du batiment
prop_ville_batimentsAdj :: Ville -> Bool
prop_ville_batimentsAdj (Ville zones _) = Map.foldrWithKey (\_ zone acc -> acc && isAdjacentToRoad zone zones) True zones
  where
    isAdjacentToRoad :: Zone -> Map ZoneId Zone -> Bool
    isAdjacentToRoad zone zonesMap = case zone of
        Route _ -> True  -- Les routes sont toujours ok
        _ -> any (zoneAdjacent zone) (Map.elems zonesMap)

    zoneAdjacent :: Zone -> Zone -> Bool
    zoneAdjacent z1 (Route f2) = adjacentes (zoneForme z1) f2
    zoneAdjacent _ _ = False

-- Un invariant pour ville
prop_inv_Ville :: Ville -> Bool
prop_inv_Ville v = prop_ville_sansCollision v && prop_ville_routesAdj v 

-- Fonction qui construit une ville en ajoutant une zone
construitville :: Ville -> Zone -> ZoneId -> Ville
construitville (Ville zones citoyens) zone zid =
    Ville (Map.insert zid zone zones) citoyens

-- Preconditions pour la fonction construit
prop_pre_construitville :: Ville -> Zone -> ZoneId -> Bool
prop_pre_construitville (Ville zones _) zone zid =
    not (Map.member zid zones) && -- L'id de la zone n'est pas déjà utilisé    
    all (\(_, z') -> not (collision (zoneForme zone) (zoneForme z'))) (Map.toList zones) &&  -- La nouvelle zone ne doit pas entrer en collision avec les zones existantes
    case zone of
        Route _ -> True  -- Les routes peuvent être placées n'importe où
        _ -> any (zoneAdjacent zone) (Map.elems zones)  -- Les autres zones doivent être adjacentes à une route
        where
            zoneAdjacent :: Zone -> Zone -> Bool
            zoneAdjacent z1 (Route f2) = adjacentes (zoneForme z1) f2
            zoneAdjacent _ _ = False


-- Postconditions pour la fonction construit
prop_post_construitville :: Ville -> Zone -> ZoneId -> Ville -> Bool
prop_post_construitville (Ville zones _) _ zid (Ville zones' _) =
    Map.member zid zones' &&  -- La zonne non reperterorier
    Map.size zones' == Map.size zones + 1  -- Il y a une zone de plus

