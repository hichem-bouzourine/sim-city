{-# LANGUAGE InstanceSigs #-}
module Batiment where
import Forme
-- import Citoyen
import Utils

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

data Batiment =
    Cabane Forme Coord Int [CitId]
    | Atelier Forme Coord Int [CitId]
    | Epicerie Forme Coord Int [CitId]
    | Commissariat Forme Coord

-- Instaciation EQ pour les Batiments
instance Eq Batiment where
    (==) :: Batiment -> Batiment -> Bool
    (Cabane f1 c1 cap1 l1) == (Cabane f2 c2 cap2 l2) = f1 == f2 && c1 == c2 && cap1 == cap2 && l1 == l2
    (Atelier f1 c1 cap1 l1) == (Atelier f2 c2 cap2 l2) = f1 == f2 && c1 == c2 && cap1 == cap2 && l1 == l2
    (Epicerie f1 c1 cap1 l1) == (Epicerie f2 c2 cap2 l2) = f1 == f2 && c1 == c2 && cap1 == cap2 && l1 == l2
    (Commissariat f1 c1) == (Commissariat f2 c2) = f1 == f2 && c1 == c2
    _ == _ = False

-- Getters de forme pour les Batiments
batimentForme :: Batiment -> Forme
batimentForme (Cabane f _ _ _) = f
batimentForme (Atelier f _ _ _) = f
batimentForme (Epicerie f _ _ _) = f
batimentForme (Commissariat f _) = f

-- Getters de coordonnées de l'entrée pour les Batiments
batimentEntree :: Batiment -> Coord
batimentEntree (Cabane _ c _ _) = c
batimentEntree (Atelier _ c _ _) = c
batimentEntree (Epicerie _ c _ _) = c
batimentEntree (Commissariat _ c) = c

-- Getters de capacité pour les Batiments
batimentCapacite :: Batiment -> Int
batimentCapacite (Cabane _ _ cap _) = cap
batimentCapacite (Atelier _ _ cap _) = cap
batimentCapacite (Epicerie _ _ cap _) = cap
batimentCapacite (Commissariat _ _) = 0

-- Getters de citoyens pour les Batiments
batimentCitoyens :: Batiment -> [CitId]
batimentCitoyens (Cabane _ _ _ l) = l
batimentCitoyens (Atelier _ _ _ l) = l
batimentCitoyens (Epicerie _ _ _ l) = l
batimentCitoyens (Commissariat _ _) = []

-- Invariant: l'entrée des bâtiments n'est pas dans leur forme et est adjacente à leur forme
prop_inv_entre_batiment :: Batiment -> Bool
prop_inv_entre_batiment b =
    let (c, f) = (batimentEntree b ,batimentForme b) in not (appartient c f) && adjacent c f

-- Invariant: le nombre de citoyens d'un bâtiment est inférieur à la capacité du bâtiment
prop_inv_capacite_batiment :: Batiment -> Bool
prop_inv_capacite_batiment (Commissariat _ _) = True
prop_inv_capacite_batiment batiment = length (batimentCitoyens batiment) <= batimentCapacite batiment

-- Invariant: les citoyens d'un bâtiment sont distincts
prop_inv_citoyens_distincts :: Batiment -> Bool
prop_inv_citoyens_distincts (Commissariat _ _) = True
prop_inv_citoyens_distincts b = length (batimentCitoyens b) == length (Set.fromList (batimentCitoyens b))

-- Invariant: Batiment
prop_inv_Batiment :: Batiment -> Bool
prop_inv_Batiment b = prop_inv_entre_batiment b && prop_inv_capacite_batiment b && prop_inv_citoyens_distincts b

-- construitBatiment un bâtiment en ajoutant un citoyen, si possible
construitBatiment:: Batiment -> CitId -> Batiment
construitBatiment (Cabane f c cap l) cid = Cabane f c cap (cid:l)
construitBatiment (Atelier f c cap l) cid = Atelier f c cap (cid:l)
construitBatiment (Epicerie f c cap l) cid = Epicerie f c cap (cid:l)
construitBatiment b _ = b

-- Précondition pour la fonction construitBatiment
-- Vérifie que le bâtiment peut ajouter un citoyen donné
prop_pre_construitBatiment :: Batiment -> CitId -> Bool
prop_pre_construitBatiment (Commissariat _ _) _ = True
prop_pre_construitBatiment batiment cid =
    batimentCapacite batiment > length (batimentCitoyens batiment)
    && notElem cid (batimentCitoyens batiment)

-- Postcondition pour la fonction construitBatiment
prop_post_construitBatiment :: Batiment -> Batiment -> CitId -> Bool
prop_post_construitBatiment (Commissariat _ _) (Commissariat _ _) _ = True
prop_post_construitBatiment b1 b2 cid =
    batimentForme b1 == batimentForme b2 &&
    batimentEntree b1 == batimentEntree b2 &&
    batimentCapacite b1 == batimentCapacite b2 &&
    cid `elem` batimentCitoyens b2 &&
    length (batimentCitoyens b2) == length (batimentCitoyens b1) + 1

-- Suppression d'un citoyen dans un bâtiment
supprimeCitoyenBatiment :: Batiment -> CitId -> Batiment
supprimeCitoyenBatiment (Cabane f c cap l) cid = Cabane f c cap (filter (/= cid) l)
supprimeCitoyenBatiment (Atelier f c cap l) cid = Atelier f c cap (filter (/= cid) l)
supprimeCitoyenBatiment (Epicerie f c cap l) cid = Epicerie f c cap (filter (/= cid) l)
supprimeCitoyenBatiment b _ = b  -- Pas de suppression pour les Commissariats ou types non spécifiés

-- Précondition pour la suppression d'un citoyen d'un bâtiment
prop_pre_supprimeCitoyenBatiment :: Batiment -> CitId -> Bool
prop_pre_supprimeCitoyenBatiment b cid = cid `elem` batimentCitoyens b

-- Postcondition pour la suppression d'un citoyen d'un bâtiment
prop_post_supprimeCitoyenBatiment :: Batiment -> Batiment -> CitId -> Bool
prop_post_supprimeCitoyenBatiment b1 b2 cid =
    notElem cid (batimentCitoyens b2) &&
    length (batimentCitoyens b2) == length (batimentCitoyens b1) - 1

-- instancer show pour Batiments
instance Show Batiment where
    show ( Cabane f c cap l ) = "Cabane " ++ show f ++ " " ++ show c ++ " " ++ show cap ++ " " ++ show l
    show ( Atelier f c cap l ) = "Atelier " ++ show f ++ " " ++ show c ++ " " ++ show cap ++ " " ++ show l
    show ( Epicerie f c cap l ) = "Epicerie " ++ show f ++ " " ++ show c ++ " " ++ show cap ++ " " ++ show l
    show ( Commissariat f c ) = "Commissariat " ++ show f ++ " " ++ show c