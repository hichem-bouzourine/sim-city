{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Componnent.Environnement

where
import Componnent.Forme
import Componnent.Utils
import Componnent.Citoyen
import Componnent.Batiment
import Componnent.Ville
import Componnent.Zone

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Componnent.Zone (zoneBatiments)
import Data.Foldable
import qualified Data.Sequence as Seq

data Environnement = Env {
    height :: Int,                              -- Hauteur de la carte
    width :: Int,                               -- Largeur de la carte
    envBatiments :: Map BatId Batiment,         -- Batiments de la ville
    eville :: Ville,                            -- Ville
    eCarte :: Map Coord Char                    -- Carte de la ville 
}

-- Getters Ville
envVille :: Environnement -> Ville
envVille (Env _ _ _ v _) = v

-- Getters Map
envMap :: Environnement -> Map Coord Char
envMap (Env _ _ _ _ m) = m

-- Cette fonction permet de d'initialiser un environnement
initEnv :: Int -> Int -> Environnement
initEnv h w = Env h w Map.empty (Ville Map.empty Map.empty) Map.empty

-- Cette fonction permet d'ajouter une zone a l'environnement
envAddZone :: ZoneId -> Zone -> Environnement -> Maybe Environnement
envAddZone zid z env@(Env h w b v m)
    -- Si les limites de la zone sont en dehors de la carte, on retourne Nothing
    | not (contentSize (zoneForme z) h w) = Nothing
    | otherwise = 
        let newMap = Map.union m (zoneMap z)
            newVille = construitville v z zid
        in case newVille of
            Just ville -> Just $ Env h w b ville newMap
            Nothing -> Nothing


-- Cette fonction permet de mettre d'ajouter un batiment dans l'environ
envAddBatiment :: BatId -> Batiment -> ZoneId -> Environnement -> Maybe Environnement
envAddBatiment bid bat zid (Env h w b v m) =
            let newMap = Map.union (batimentMap bat) m
                newVille = case getZoneWithId v zid of
                    Just z ->
                        -- on verifie si l'entree du batiment est ajaçant a une route
                        if any (appartient (batimentEntree bat) . zoneForme) (villeZoneRoutes v)
                            then case construitZone z bat of
                                    Just zone -> case construitville v zone zid of
                                        Just ville -> Just ville
                                        _ -> Nothing
                                    Nothing -> Nothing
                            else Nothing
                    Nothing -> Nothing
                    in 
                        case newVille of
                            Just ville -> Just (Env h w (Map.insert bid bat b) ville newMap)
                            Nothing -> Nothing

-- Cette fonction permet d'ajouter un citoyen dans l'environnement
envAddCitoyen :: CitId -> Citoyen -> Environnement -> Environnement
envAddCitoyen cid cit (Env h w b (Ville z c) m) = Env h w b (Ville z (Map.insert cid cit c)) newMap
    where
        newMap = Map.insert (citoyenCoord cit) 'X' m

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


-- Cette fonction permet de trouver une porte pour une forme de batiment 
-- closestForm :: Forme -> [Forme] -> Maybe Coord

findDoor :: Forme -> Environnement -> Maybe Coord 
findDoor forme env = let routes =  Map.elems $ extractRoutes (envVille env) in closestForm forme routes

-- Cette fonction renvoie une zone et son id en cherchant la zone qui est definie sur la condonée 
getZoneWithCoord :: Environnement -> Coord -> Maybe (ZoneId, Zone)
getZoneWithCoord env coord = let zones = viZones (envVille env) in
    case Map.toList zones of
        [] -> Nothing
        _ -> foldr (\(zid, z) acc -> if appartient coord (zoneForme z) then Just (zid, z) else acc) Nothing (Map.toList zones)
-- Cette fonction permet de recuperer un batiment à partir de son identifiant
getBatimentWithId :: Environnement -> BatId -> Batiment
getBatimentWithId env bid = case Map.lookup bid (envBatiments env) of
    Just b -> b
    Nothing -> error "Batiment not found"

-- Cette fonction teste si un batiment est dans l'environnement
isBatimentInEnv ::Environnement -> Batiment -> Bool
isBatimentInEnv env bat = bat `elem` Map.elems (envBatiments env)


-- Cette fonction permet recuperer un batiment a partir de son identifiant
getBatIdFromBatiment :: Batiment -> Environnement -> Maybe BatId
getBatIdFromBatiment batiment env =
    Map.foldrWithKey (\k v acc -> if v == batiment then Just k else acc) Nothing (envBatiments env)

-- Cette fonction permet de mettre a jour un batiment dans l'environnement
putBatimentWithId :: BatId -> Batiment -> Environnement -> Environnement
putBatimentWithId bid bat (Env h w b v m) = Env h w (Map.insert bid bat b) v m

-- Cette fonction renvoie un batiment de travaille disponible dans la liste des batiments
getBatimentTravail :: Environnement -> Maybe (BatId, Batiment)
getBatimentTravail env = Map.foldrWithKey (\k b acc -> if estBatimentTravail b then Just (k, b) else acc) Nothing (envBatiments env)

-- Cette fonction renvoie un batiment de repos disponible dans la liste des batiments
getBatimentRepos :: Environnement -> Maybe (BatId, Batiment)
getBatimentRepos env = Map.foldrWithKey (\k b acc -> if estBatimentRepos b then Just (k, b) else acc) Nothing (envBatiments env)

-- Cette fonction renvoie un batiment de course disponible dans la liste des batiments
getBatimentCourse :: Environnement -> Maybe (BatId, Batiment)
getBatimentCourse env = Map.foldrWithKey (\k b acc -> if estBatimentCommerce b then Just (k, b) else acc) Nothing (envBatiments env)

-- Cette fonction renvoie les commissariats dans la liste des batiments
getCommissariats :: Environnement -> Maybe  (BatId, Batiment)
getCommissariats env = Map.foldrWithKey (\k b acc -> if estBatimentCommissariat b then Just (k, b) else acc) Nothing (envBatiments env)

-- Cette fonction renvoie une coordonner d'une route pour au hasard dans la ville
isRoadPoint :: Environnement -> Coord -> Bool
-- verifier si le point appartien a une route 
isRoadPoint env coord = let routes =  Map.elems $ extractRoutes (envVille env) in any (\l -> appartient coord l) routes

-- cette fonction permet d'initialiser la map par rapport a une ville
initMap ::  Ville -> Map Coord Char
initMap  ville =
    let zMap = foldr (\z acc -> Map.union acc (zoneMap z)) Map.empty (villeZones ville)
    in foldr (\c acc -> Map.insert (citoyenCoord c) 'X' acc) zMap (villeCitoyens ville)

        

-- Cette fonction permet de transformer une carte en tableau a deux dimentsion donc 
-- chaque case est indexée par un couple Coord et contient une chaine de caractère la valeur de coord dans la map
mapToTable :: Int -> Int -> Map Coord Char -> [[Char]]
mapToTable h l coordsMap =
    let coordsMapRevers = Map.fromList $ map (\(C x y, c) -> (C x (h - y - 1), c)) (Map.toList coordsMap)in
    map (\y -> map (\x -> Map.findWithDefault ' ' (C x y) coordsMapRevers) [0..l-1]) [0..h-1]


zonneTest = Map.fromList [(ZoneId 3, Route (HSegement (C (-5) 5) 5))]

villeTest = Ville zonneTest (Map.fromList [(CitId "1", Emigrant (C 0 0) Travailler), (CitId "2", Emigrant (C 1 1) Travailler)])

-- >>> initMap  villeTest
-- fromList [(C {cx = -5, cy = 5},'#'),(C {cx = -4, cy = 5},'#'),(C {cx = -3, cy = 5},'#'),(C {cx = -2, cy = 5},'#'),(C {cx = -1, cy = 5},'#'),(C {cx = 0, cy = 0},'X'),(C {cx = 0, cy = 5},'#'),(C {cx = 1, cy = 1},'X')]

-- Cette fonctin prend un tableau a deux dimension en une chaine de caracte representant la carte
-- la bordure du tableau a gauche et droite par '|' et les bordure haut et bas par '_'
tableToString :: [[Char]] -> String
tableToString table@(firstLine:_) =
    let topBottomBorder = " " ++ replicate (length firstLine) '_' ++ " "
    in unlines $ [topBottomBorder] ++ map (\line -> "|" ++ line ++ "|") table ++ [topBottomBorder]
tableToString [] = ""


-- Cette fonction permet de transformer un environnement en chaine de caractère
showEnvironment :: Environnement -> String
showEnvironment env = tableToString $ mapToTable (height env) (width env) (envMap env)

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
cFaim = 1

-- define Show instance for Environnement
instance Show Environnement where
    show (Env h w b v m) = "Environnement { height = " ++ show h ++ ", width = " ++ show w ++ ", envBatiments = " ++ show b ++ ", ville = " ++ show v ++ ", eCarte = " ++ show m ++ " }"