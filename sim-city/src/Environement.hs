{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Environement where
import Forme
import Utils
import Citoyen
import Batiment
import Ville

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Zone (zoneBatiments)
import Data.Foldable

data Environement = Env {
    height :: Int,
    width :: Int,
    envBatiments :: Map BatId Batiment,
    ville :: Ville,
    viMap :: Map Coord String
}

-- Getters Ville
envVille :: Environement -> Ville
envVille (Env _ _ _ v _) = v

-- | Invariant: tout les batiments de la ville sont dans l'environement envBatiments
prop_inv_citoyen_batiment :: Environement -> Bool
prop_inv_citoyen_batiment env = foldr aux True (villeBatiments (envVille env))
    where
        aux :: Batiment -> Bool -> Bool
        aux b acc = acc && isBatimentInEnv env b
    
-- | Invariant: chaque bâtiment associé à un citoyen contient ce citoyen dans sa liste de citoyens.
prop_inv_citoyen_batiment_citoyen :: Environement -> Bool
prop_inv_citoyen_batiment_citoyen (Env _ _ envBatiments (Ville _ citoyens)_) =
    Map.foldrWithKey (\cid cit acc -> acc && all (citoyenInBatiment cid) (citoyenBatiments cit)) True citoyens
  where
    -- Vérifie si un citoyen est dans la liste des citoyens d'un bâtiment
    citoyenInBatiment :: CitId -> BatId -> Bool
    citoyenInBatiment cid bid = case Map.lookup bid envBatiments of
        Just b -> cid `elem` batimentCitoyens b
        Nothing -> False

-- | Invariant: chaque citoyen associé à un bâtiment a une reference vers ce bâtiment dans ça liste de batId.
prop_inv_citoyen_batiment_citoyen_batiment :: Environement -> Bool
prop_inv_citoyen_batiment_citoyen_batiment (Env _ _ envBatiments (Ville _ citoyens)_) =
    Map.foldrWithKey (\bid b acc -> acc && all (batimentInCitoyen bid) (batimentCitoyens b)) True envBatiments
  where
    -- Vérifie si un bâtiment est dans la liste des bâtiments d'un citoyen
    batimentInCitoyen :: BatId -> CitId -> Bool
    batimentInCitoyen bid cid = case Map.lookup cid citoyens of
        Just c -> bid `elem` citoyenBatiments c
        Nothing -> False

-- Cette fonction permet de recuperer un batiment à partir de son identifiant
getBatimentWithId :: Environement -> BatId -> Batiment
getBatimentWithId env bid = case Map.lookup bid (envBatiments env) of
    Just b -> b
    Nothing -> error "Batiment not found"

-- Cette fonction teste si un batiment est dans l'environement
isBatimentInEnv ::Environement -> Batiment -> Bool
isBatimentInEnv env bat = bat `elem` Map.elems (envBatiments env)


-- | This function retrieves the BatId for a given building from the environment.
getBatIdFromBatiment :: Batiment -> Environement -> Maybe BatId
getBatIdFromBatiment batiment env =
    Map.foldrWithKey (\k v acc -> if v == batiment then Just k else acc) Nothing (envBatiments env)

-- Cette fonction permet de mettre a jour un batiment dans l'environement
putBatimentWithId :: BatId -> Batiment -> Environement -> Environement
putBatimentWithId bid bat env = env { envBatiments = Map.insert bid bat (envBatiments env) }

-- cette fonction permet d'initialiser la map par rapport a une ville
initMap ::  Ville -> Map Coord String
initMap  ville= 
    let zMap = foldr (\z acc -> Map.union acc (zoneMap z)) Map.empty (villeZones ville)
    in Map.empty
        

-- >>> initMap  (Map.fromList [(BatId 5, Commissariat (Rectangle (C 0 0) 5 5) (C 0 0)), (BatId 6, Commissariat (Rectangle (C 0 0) 5 5) (C 0 0))]) 
-- fromList [(C {cx = 0, cy = -5},"*"),(C {cx = 0, cy = -4},"*"),(C {cx = 0, cy = -3},"*"),(C {cx = 0, cy = -2},"*"),(C {cx = 0, cy = -1},"*"),(C {cx = 0, cy = 0},"*"),(C {cx = 1, cy = -5},"*"),(C {cx = 1, cy = 0},"*"),(C {cx = 2, cy = -5},"*"),(C {cx = 2, cy = 0},"*"),(C {cx = 3, cy = -5},"*"),(C {cx = 3, cy = 0},"*"),(C {cx = 4, cy = -5},"*"),(C {cx = 4, cy = 0},"*"),(C {cx = 5, cy = -5},"*"),(C {cx = 5, cy = -4},"*"),(C {cx = 5, cy = -3},"*"),(C {cx = 5, cy = -2},"*"),(C {cx = 5, cy = -1},"*"),(C {cx = 5, cy = 0},"*")]

-- Parametres des citoyens
wFatigue :: Int
wFatigue = -2

wFaim :: Int
wFaim = - 1

wGain :: Int
wGain = 3

dFatigue :: Int
dFatigue = 5

cFaim :: Int
cFaim = -1

