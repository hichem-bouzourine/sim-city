{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Environement where
import Forme
import Utils
import Citoyen
import Batiment
import Ville

import Data.Map (Map)
import qualified Data.Map as Map

data Environement = Env {
    height :: Int,
    width :: Int,
    envBatiments :: Map BatId Batiment,
    ville :: Ville
}

-- Getters Ville
envVille :: Environement -> Ville
envVille (Env _ _ _ v) = v

-- | Invariant: tout les batiments de la ville sont dans l'environement envBatiments
prop_inv_citoyen_batiment :: Environement -> Bool
prop_inv_citoyen_batiment (Env _ _ envBatiments ville) = 
    all (\b -> b `elem` Map.elems envBatiments) (villeBatiments ville)

-- | Invariant: chaque bâtiment associé à un citoyen contient ce citoyen dans sa liste de citoyens.
prop_inv_citoyen_batiment_citoyen :: Environement -> Bool
prop_inv_citoyen_batiment_citoyen (Env _ _ envBatiments (Ville _ citoyens)) = 
    Map.foldrWithKey (\cid cit acc -> acc && all (citoyenInBatiment cid) (citoyenBatiments cit)) True citoyens
  where
    -- Vérifie si un citoyen est dans la liste des citoyens d'un bâtiment
    citoyenInBatiment :: CitId -> BatId -> Bool
    citoyenInBatiment cid bid = case Map.lookup bid envBatiments of
        Just b -> cid `elem` batimentCitoyens b
        Nothing -> False

-- | Invariant: chaque citoyen associé à un bâtiment a une reference vers ce bâtiment dans ça liste de batId.
prop_inv_citoyen_batiment_citoyen_batiment :: Environement -> Bool
prop_inv_citoyen_batiment_citoyen_batiment (Env _ _ envBatiments (Ville _ citoyens)) = 
    Map.foldrWithKey (\bid b acc -> acc && all (batimentInCitoyen bid) (batimentCitoyens b)) True envBatiments
  where
    -- Vérifie si un bâtiment est dans la liste des bâtiments d'un citoyen
    batimentInCitoyen :: BatId -> CitId -> Bool
    batimentInCitoyen bid cid = case Map.lookup cid citoyens of
        Just c -> bid `elem` citoyenBatiments c
        Nothing -> False

-- Cette fonction permet de recuperer un batiment à partir de son identifiant
getBatimentWithId :: Environement -> BatId -> Maybe Batiment
getBatimentWithId env bid = Map.lookup bid (envBatiments env)

-- | This function retrieves the BatId for a given building from the environment.
getBatIdFromBatiment :: Batiment -> Environement -> Maybe BatId
getBatIdFromBatiment batiment env = 
    Map.foldrWithKey (\k v acc -> if v == batiment then Just k else acc) Nothing (envBatiments env)

-- Cette fonction permet de mettre a jour un batiment dans l'environement
putBatimentWithId :: BatId -> Batiment -> Environement -> Environement
putBatimentWithId bid bat env = env { envBatiments = Map.insert bid bat (envBatiments env) }

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

