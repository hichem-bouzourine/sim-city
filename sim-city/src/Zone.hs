module Zone where
import Forme 

import Data.Map (Map)
import qualified Data.Map as Map

-- Définitions des zones et bâtiments
data Batiment = Batiment Int deriving (Show, Eq)
data Zone = Eau Forme | Route Forme | ZR Forme [Batiment] | ZI Forme [Batiment] | ZC Forme [Batiment] | Admin Forme Batiment deriving (Show, Eq)

zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f

-- Identifiants pour les éléments de la ville
newtype ZoneId = ZoneId Int deriving (Eq, Ord)
newtype BatId = BatId Int deriving (Eq, Ord)
newtype CitId = CitId String deriving (Eq, Ord)

-- État de la ville
data Ville = Ville {
    viZones :: Map ZoneId Zone,
    viCit :: Map CitId Citoyen
}

data Citoyen = Citoyen {
    ciNom :: CitId
} 

-- Collision entre formes
collision :: Forme -> Forme -> Bool
collision _ _ = False  -- À redéfinir selon les règles de collision de votre jeu

-- Propriété de non-collision entre zones
prop_ville_sansCollision :: Ville -> Bool
prop_ville_sansCollision (Ville zs _) = Map.foldrWithKey aux True zs
  where
    aux _ _ False = False
    aux key z acc = acc && Map.foldr (\z' acc' -> acc' && (not $ collision (zoneForme z) (zoneForme z'))) True (Map.delete key zs)

-- Propriété que chaque zone est adjacente à une route
prop_ville_routesAdj :: Ville -> Bool
prop_ville_routesAdj (Ville zones _) = Map.foldrWithKey (\_ zone acc -> acc && isAdjacentToRoad zone zones) True zones
  where
    isAdjacentToRoad :: Zone -> Map ZoneId Zone -> Bool
    isAdjacentToRoad zone zonesMap = case zone of
        Route _ -> True  -- Les routes sont toujours ok
        _ -> any (zoneAdjacent zone) (Map.elems zonesMap)

    zoneAdjacent :: Zone -> Zone -> Bool
    zoneAdjacent z1 route@(Route f2) = any (\coord -> adjacent coord (zoneForme z1)) (boundingCoords  f2)
    zoneAdjacent _ _ = False


-- Fonction qui génère les coordonnées des deux extrémités d'un segment en utilisant `limites`
boundingCoords :: Forme -> [Coord]
boundingCoords forme = case forme of
    HSegment start l -> 
        let (n, _, w, e) = limites forme
        in [C w n, C e n]  -- Coordonnées des extrémités ouest et est
    VSegment start l -> 
        let (n, s, w, _) = limites forme
        in [C w n, C w s]  -- Coordonnées des extrémités nord et sud
    _ -> []  -- Pour les rectangles ou autres formes non segmentées

-- Un invariant pour ville
prop_inv_Ville :: Ville -> Bool
prop_inv_Ville v = prop_ville_sansCollision v && prop_ville_routesAdj v

-- Fonction qui construit une ville en ajoutant une zone
construit :: Ville -> Zone -> ZoneId -> Ville
construit (Ville zones citoyens) zone zid =
    Ville (Map.insert zid zone zones) citoyens

-- Preconditions pour la fonction construit
prop_pre_construit :: Ville -> Zone -> ZoneId -> Bool
prop_pre_construit (Ville zones _) zone zid = 
    not (Map.member zid zones) && -- L'id de la zone n'est pas déjà utilisé    
    all (\(_, z') -> not (collision (zoneForme zone) (zoneForme z'))) (Map.toList zones) &&  -- La nouvelle zone ne doit pas entrer en collision avec les zones existantes
    case zone of
        Route _ -> True  -- Les routes peuvent être placées n'importe où
        _ -> any (\ z' -> zoneAdjacent zone z') (Map.elems zones)  -- Les autres zones doivent être adjacentes à une route
        where
            zoneAdjacent :: Zone -> Zone -> Bool
            zoneAdjacent z1 route@(Route f2) = any (\coord -> adjacent coord (zoneForme z1)) (boundingCoords  f2)
            zoneAdjacent _ _ = False


-- Postconditions pour la fonction construit
prop_post_construit :: Ville -> Zone -> ZoneId -> Ville -> Bool
prop_post_construit (Ville zones _) zone zid (Ville zones' _) = 
    Map.member zid zones' &&  -- La zone a bien été ajoutée
    Map.size zones' == Map.size zones + 1  -- Il y a une zone de plus
