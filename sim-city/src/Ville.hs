{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ville where
import Forme
import Citoyen
import Utils
import Batiment
import Zone
import Data.Map (Map)
import qualified Data.Map as  Map
import qualified Data.Set as Set

-- État de la ville
data Ville = Ville {
    viZones :: Map ZoneId Zone,
    viCit :: Map CitId Citoyen
}

-- Getters pour les Batiments de la ville
villeBatiments :: Ville -> [Batiment]
villeBatiments (Ville zones _) = Map.foldr aux [] zones
  where
    aux :: Zone -> [Batiment] -> [Batiment]
    aux z acc = zoneBatiments z ++ acc

-- Getters des zones d'une villes 
villeZones :: Ville -> [Zone]
villeZones (Ville zones _) = Map.elems zones

-- Getters ville citoyen
villeCitoyens :: Ville -> [Citoyen]
villeCitoyens (Ville _ citoyens) = Map.elems citoyens

-- Getters pour les BatId concernés par les citoyens
villeCitoyensBatId :: Ville -> [BatId]
villeCitoyensBatId (Ville _ citoyens) = Map.foldr aux [] citoyens
  where
    aux :: Citoyen -> [BatId] -> [BatId]
    aux c acc = citoyenBatiments c ++ acc

-- Getters pour les CitId concernés par les batiments
villeBatimentsCitId :: Ville -> [CitId]
villeBatimentsCitId (Ville zones _) = Map.foldr aux [] zones
  where
    aux :: Zone -> [CitId] -> [CitId]
    aux z acc = foldr (\b acc' -> batimentCitoyens b ++ acc') acc (zoneBatiments z)

-- Cette fonction permet de recuperer un Citoyen 
villeGetCitoyen :: Ville -> CitId ->  Citoyen
villeGetCitoyen (Ville _ citoyens) cid = case Map.lookup cid citoyens of
    Just c -> c
    Nothing -> error "Citoyen non trouvé"

-- Getter pour les CitId de la ville
villeCitIds :: Ville -> [CitId]
villeCitIds (Ville _ citoyens) = Map.keys citoyens

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

-- Vérifie que chaque bâtiment est adjacent à une route
prop_ville_batimentsAdj :: Ville -> Bool
prop_ville_batimentsAdj (Ville zones _) = all batimentsAdjacentsARoute (Map.elems zones)
  where
    -- Vérifie que tous les bâtiments dans une zone sont adjacents à une route
    batimentsAdjacentsARoute :: Zone -> Bool
    batimentsAdjacentsARoute zone = all bâtimentAdjacentRoute (zoneBatiments zone)

    -- Détermine si un bâtiment est adjacent à une route dans la ville
    bâtimentAdjacentRoute :: Batiment -> Bool
    bâtimentAdjacentRoute batiment = any (adjacentARoute batiment) (Map.elems zones)

    -- Vérifie si un bâtiment est adjacent à une zone routière
    adjacentARoute :: Batiment -> Zone -> Bool
    adjacentARoute batiment (Route formeRoute) = adjacentes (batimentForme batiment) formeRoute
    adjacentARoute _ _ = False

-- toute les porte des batiments sont adjacente a une route
prop_ville_portesAdj :: Ville -> Bool
prop_ville_portesAdj (Ville zones _) = all batimentsPortesAdjacentsARoute (Map.elems zones)
  where
    -- Vérifie que tous les bâtiments dans une zone sont adjacents à une route
    batimentsPortesAdjacentsARoute :: Zone -> Bool
    batimentsPortesAdjacentsARoute zone = all bâtimentPorteAdjacentRoute (zoneBatiments zone)

    -- Détermine si un bâtiment est adjacent à une route dans la ville
    bâtimentPorteAdjacentRoute :: Batiment -> Bool
    bâtimentPorteAdjacentRoute batiment = all (porteAdjacentRoute batiment) (Map.elems zones)

    -- Vérifie si un bâtiment est adjacent à une zone routière
    porteAdjacentRoute :: Batiment -> Zone -> Bool
    porteAdjacentRoute batiment (Route formeRoute) = adjacent (batimentEntree batiment) formeRoute
    porteAdjacentRoute _ _ = False

-- Invariant: Tous les citoyens dans les bâtiments doivent faire partie des citoyens de la ville
prop_inv_citoyensDansVille :: Ville -> Bool
prop_inv_citoyensDansVille (Ville zones citoyens) =
    all citoyensDansVille (Map.elems zones)
  where
    citoyensDansVille :: Zone -> Bool
    citoyensDansVille zone = all (\b -> Set.fromList (batimentCitoyens b) `Set.isSubsetOf` citoyenIDs) (zoneBatiments zone)

    citoyenIDs :: Set.Set CitId
    citoyenIDs = Map.keysSet citoyens

-- Invariant: Tous les batiments d'une ville est distinct
prop_inv_batimentsDistincts :: Ville -> Bool
prop_inv_batimentsDistincts ville = foldr (\b acc -> acc && batimentDistinct b) True (villeBatiments ville)
  where
    batimentDistinct :: Batiment -> Bool
    batimentDistinct b = length (filter (b ==) (villeBatiments ville)) == 1

-- ? routes connexes ! :
estAdjacenteARoute :: Zone -> Ville -> Bool
estAdjacenteARoute z ville = any (\z' -> case z' of 
        Route _ -> adjacentes (zoneForme z) (zoneForme z'); 
        _ -> False) (Map.elems (viZones ville))

-- Vérifie si une liste de zones est une séquence de zones de route connexes, en utilisant estAdjacenteARoute
estRouteConnexe :: [Zone] -> Ville -> Bool
estRouteConnexe [] _ = True
estRouteConnexe [_] _ = True
estRouteConnexe (z1:z2:zs) ville = estAdjacenteARoute z1 ville && estRouteConnexe (z2:zs) ville


-- Vérifie si les Routes d'une ville sont connexes donc on filtre les routes puis on vérifie s'il sont connexes,
routesConnexes :: Ville -> Bool
routesConnexes (Ville zones _) = estRouteConnexe (filter (\z -> case z of Route _ -> True; _ -> False) (Map.elems zones)) (Ville zones Map.empty)

-- Un invariant pour ville
prop_inv_Ville :: Ville -> Bool
prop_inv_Ville v = prop_ville_sansCollision v && prop_ville_routesAdj v &&  prop_inv_batimentsDistincts v
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
    Map.member zid zones' &&  -- La zonne non reperterorier  -- La zonne non reperterorier  -- La zonne non reperterorier  -- La zonne non reperterorier
      -- La zonne non reperterorier
    Map.size zones' == Map.size zones + 1  -- Il y a une zone de plus

-- Fonction qui determine le taux de chomage dans une ville
tauxChomage :: Ville -> Float
tauxChomage (Ville _ citoyens) = fromIntegral (length (filter estChomeur (Map.elems citoyens))) / fromIntegral (Map.size citoyens)
  where
    estChomeur :: Citoyen -> Bool
    estChomeur c = citoyenOccupation c /= Travailler

-- Fonction qui supprime une zone dans une ville
supprimeZone :: Ville -> ZoneId -> Ville
supprimeZone (Ville zones citoyens) zid =
    Ville (Map.delete zid zones) citoyens

-- Précondition pour la suppression d'une zone
prop_pre_supprimeZone :: Ville -> ZoneId -> Bool
prop_pre_supprimeZone (Ville zones _) zid =
    Map.member zid zones  -- La zone doit exister pour être supprimée

-- Postcondition pour la suppression d'une zone
prop_post_supprimeZone :: Ville -> Ville -> ZoneId -> Bool
prop_post_supprimeZone (Ville zones _) (Ville zones' _) zid =
    not (Map.member zid zones') &&  -- La zone doit avoir été retirée  -- La zone doit avoir été retirée  -- La zone doit avoir été retirée  -- La zone doit avoir été retirée
      -- La zone doit avoir été retirée
    Map.size zones' == Map.size zones - 1  -- Il doit y avoir une zone de moins

-- Fonction qui ajoute un citoyen à une ville
ajouteCitoyen :: Ville -> Citoyen -> CitId -> Ville
ajouteCitoyen (Ville zones citoyens) cit cid =
    Ville zones (Map.insert cid cit citoyens)

-- Précondition pour l'ajout d'un citoyen
prop_pre_ajouteCitoyen :: Ville -> Citoyen -> CitId -> Bool
prop_pre_ajouteCitoyen (Ville _ citoyens) _ cid =
    not (Map.member cid citoyens)  -- L'id du citoyen ne doit pas déjà être utilisé

-- Postcondition pour l'ajout d'un citoyen
prop_post_ajouteCitoyen :: Ville -> Ville -> Citoyen -> CitId -> Bool
prop_post_ajouteCitoyen (Ville _ citoyens) (Ville _ citoyens' ) cit cid =
    Map.lookup cid citoyens' == Just cit &&  -- Le citoyen doit être ajouté 
    Map.size citoyens' == Map.size citoyens + 1  -- Il doit y avoir un citoyen de plus

-- | Retrieves the zone from the city to which a building belongs.
getZoneBatiment :: Ville -> Batiment -> Maybe (ZoneId, Zone)
getZoneBatiment (Ville zones _) batiment = 
    Map.foldrWithKey (\k z acc -> if batiment `elem` zoneBatiments z then Just (k, z) else acc) Nothing zones

-- define Show ZoneId
instance Show ZoneId where
    show (ZoneId i) = "ZoneId " ++ show i


-- define Show for Ville 
instance Show Ville where
    show (Ville zones citoyens) = "Ville {zones = " ++ show zones ++ ", citoyens = " ++ show citoyens ++ "}"