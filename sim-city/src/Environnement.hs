{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Environnement 
   
where
import Forme
import Utils
import Citoyen
import Batiment
import Ville
import Zone

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Zone (zoneBatiments)
import Data.Foldable
import Debug.Trace (trace)

data Environnement = Env {
    height :: Int,                          -- Hauteur de la carte
    width :: Int,                           -- Largeur de la carte
    envBatiments :: Map BatId Batiment,     -- Liste des bâtiments de la ville
    eville :: Ville,                        -- Ville
    eCarte :: Map Coord Char                -- Carte de la ville
}

-- Getters Ville
envVille :: Environnement -> Ville
envVille (Env _ _ _ v _) = v

-- Getters Map
envMap :: Environnement -> Map Coord Char
envMap (Env _ _ _ _ m) = m

-- Getter envBatiments
getBatiments :: Environnement -> Map BatId Batiment
getBatiments (Env _ _ b _ _) = b

-- | Invariant: tout les batiments de la ville sont dans l'environnement envBatiments
prop_inv_citoyen_batiment :: Environnement -> Bool
prop_inv_citoyen_batiment env = foldr aux True (villeBatiments (envVille env))
    where
        aux :: Batiment -> Bool -> Bool
        aux b acc = acc && isBatimentInEnv env b

-- | Invariant: chaque bâtiment associé à un citoyen contient ce citoyen dans sa liste de citoyens.
prop_inv_citoyen_batiment_citoyen :: Environnement -> Bool
prop_inv_citoyen_batiment_citoyen (Env _ _ envBatiments (Ville _ citoyens)_) =
    Map.foldrWithKey (\cid cit acc -> acc && all (citoyenInBatiment cid) (citoyenBatiments cit)) True citoyens
  where
    -- Vérifie si un citoyen est dans la liste des citoyens d'un bâtiment
    citoyenInBatiment :: CitId -> BatId -> Bool
    citoyenInBatiment cid bid = case Map.lookup bid envBatiments of
        Just b -> cid `elem` batimentCitoyens b
        Nothing -> False

-- | Invariant: chaque citoyen associé à un bâtiment a une reference vers ce bâtiment dans ça liste de batId.
prop_inv_citoyen_batiment_citoyen_batiment :: Environnement -> Bool
prop_inv_citoyen_batiment_citoyen_batiment (Env _ _ envBatiments (Ville _ citoyens)_) =
    Map.foldrWithKey (\bid b acc -> acc && all (batimentInCitoyen bid) (batimentCitoyens b)) True envBatiments
  where
    -- Vérifie si un bâtiment est dans la liste des bâtiments d'un citoyen
    batimentInCitoyen :: BatId -> CitId -> Bool
    batimentInCitoyen bid cid = case Map.lookup cid citoyens of
        Just c -> bid `elem` citoyenBatiments c
        Nothing -> False

-- Cette fonction permet de recuperer un batiment à partir de son identifiant
getBatimentWithId :: Environnement -> BatId -> Batiment
getBatimentWithId env bid = case Map.lookup bid (envBatiments env) of
    Just b -> b
    Nothing -> error "Batiment not found"

-- Cette fonction teste si un batiment est dans l'environnement
isBatimentInEnv ::Environnement -> Batiment -> Bool
isBatimentInEnv env bat = bat `elem` Map.elems (envBatiments env)


-- | This function retrieves the BatId for a given building from the environment.
getBatIdFromBatiment :: Batiment -> Environnement -> Maybe BatId
getBatIdFromBatiment batiment env =
    Map.foldrWithKey (\k v acc -> if v == batiment then Just k else acc) Nothing (envBatiments env)

-- Cette fonction permet de mettre a jour un batiment dans l'environnement
putBatimentWithId :: BatId -> Batiment -> Environnement -> Environnement
putBatimentWithId bid bat (Env h w b v m) = Env h w (Map.insert bid bat b) v m

-- cette fonction permet d'initialiser la map par rapport a une ville
initMap ::  Ville -> Map Coord Char
initMap  ville =
    let zMap = foldr (\z acc -> Map.union acc (zoneMap z)) Map.empty (villeZones ville)
    in foldr (\c acc -> Map.insert (citoyenCoord c) 'X' acc) zMap (villeCitoyens ville)

-- Cette fonction permet de transformer une carte en tableau a deux dimentsion donc 
-- chaque case est indexée par un couple Coord et contient une chaine de caractère la valeur de coord dans la map
mapToTable :: Int -> Int -> Map Coord Char -> [[Char]]
mapToTable h l coordsMap =
    let
        -- Calcul du centre du tableau
        centerX = l `div` 2
        centerY = h `div` 2

        -- Fonction auxiliaire pour ajuster les coordonnées par rapport au centre
        coordAdjust (C x y) = C (x + centerX) (centerY - y)

        -- Initialisation du tableau avec des espaces
        initTable = replicate h (replicate l ' ')

        -- Fonction auxiliaire pour placer une valeur dans le tableau
        placeValue table (C x y) value
            | x >= 0 && x < l && y >= 0 && y < h = take y table ++
                [take x (table !! y) ++ [value] ++ drop (x + 1) (table !! y)] ++
                drop (y + 1) table
            | otherwise = table

        -- Mise à jour du tableau avec toutes les valeurs de la carte
        filledTable = foldl (\acc (coord, value) ->
                        let adjustedCoord = coordAdjust coord
                        in placeValue acc adjustedCoord value)
                    initTable (Map.toList coordsMap)
    in
        filledTable

zonneTest = Map.fromList [(ZoneId 3, Route (HSegement (C (-5) 5) 5))]

villeTest = Ville zonneTest (Map.fromList [(CitId "1", Emigrant (C 0 0) Travailler), (CitId "2", Emigrant (C 1 1) Travailler)])

-- >>> initMap  villeTest
-- fromList [(C {cx = -5, cy = 5},'#'),(C {cx = -4, cy = 5},'#'),(C {cx = -3, cy = 5},'#'),(C {cx = -2, cy = 5},'#'),(C {cx = -1, cy = 5},'#'),(C {cx = 0, cy = 0},'X'),(C {cx = 0, cy = 5},'#'),(C {cx = 1, cy = 1},'X')]

-- Cette fonctin prend un tableau a deux dimension en une chaine de caracte representant la carte
-- la bordure du tableau a gauche et droite par '|' et les bordure haut et bas par '_'
tableToString :: [[Char]] -> String
tableToString = unlines . map (\line -> "|" ++ line ++ "|") . (:) (replicate 10 '_') . (++ [replicate 10 '_'])

-- >>> tableToString $ mapToTable 11 11 (initMap villeTest)
-- "|__________|\n|######     |\n|           |\n|           |\n|           |\n|      X    |\n|     X     |\n|           |\n|           |\n|           |\n|           |\n|           |\n|__________|\n"


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

-- define Show instance for Environnement
instance Show Environnement where
    show (Env h w b v m) = "Environnement { height = " ++ show h ++ ", width = " ++ show w ++ ", envBatiments = " ++ show b ++ ", ville = " ++ show v ++ ", eCarte = " ++ show m ++ " }"
