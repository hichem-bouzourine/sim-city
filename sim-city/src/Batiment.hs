{-# LANGUAGE InstanceSigs #-}
module Batiment where
import Forme
import Citoyen

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

-- invariants-- Invariant: l'entrée des bâtiments n'est pas dans leur forme
prop_inv_entre_batiment :: Batiment -> Bool
prop_inv_entre_batiment (Cabane f c _ _) = not (appartient c f)
prop_inv_entre_batiment (Atelier f c _ _) = not (appartient c f)
prop_inv_entre_batiment (Epicerie f c _ _) = not (appartient c f)
prop_inv_entre_batiment (Commissariat f c) = not (appartient c f)

-- Invariant: le nombre de citoyens d'un bâtiment est inférieur à la capacité du bâtiment
prop_inv_capacite_batiment :: Batiment -> Bool
prop_inv_capacite_batiment (Cabane _ _ c l) = c >= length l
prop_inv_capacite_batiment (Atelier _ _ c l) = c >= length l
prop_inv_capacite_batiment (Epicerie _ _ c l) = c >= length l
prop_inv_capacite_batiment _ = True

-- Invariant: les citoyens d'un bâtiment sont distincts
prop_inv_citoyens_distincts :: Batiment -> Bool
prop_inv_citoyens_distincts (Cabane _ _ _ l) = length l == Set.size (Set.fromList l)
prop_inv_citoyens_distincts (Atelier _ _ _ l) = length l == Set.size (Set.fromList l)
prop_inv_citoyens_distincts (Epicerie _ _ _ l) = length l == Set.size (Set.fromList l)
prop_inv_citoyens_distincts _ = True


-- construitBatiment un bâtiment en ajoutant un citoyen, si possible
construitBatiment:: Batiment -> CitId -> Batiment
construitBatiment (Cabane f c cap l) cid = Cabane f c cap (cid:l)
construitBatiment (Atelier f c cap l) cid = Atelier f c cap (cid:l)
construitBatiment (Epicerie f c cap l) cid = Epicerie f c cap (cid:l)
construitBatiment b _ = b

  
-- construitBatiment:: Batiment -> CitId -> Maybe Batiment
-- construitBatiment (Commissariat _ _) _ = Nothing
-- construitBatiment (Cabane f c cap l) cid
--   | cap > length l && notElem cid l = Just (Cabane f c cap (cid:l))
--   | otherwise = Nothing
-- construitBatiment (Atelier f c cap l) cid
--   | cap > length l && notElem cid l = Just (Atelier f c cap (cid:l))
--   | otherwise = Nothing
-- construitBatiment (Epicerie f c cap l) cid
--   | cap > length l && notElem cid l = Just (Epicerie f c cap (cid:l))
--   | otherwise = Nothing

-- Précondition pour la fonction construitBatiment
-- Vérifie que le bâtiment peut ajouter un citoyen donné
prop_pre_construitBatiment :: Batiment -> CitId -> Bool
prop_pre_construitBatiment (Commissariat _ _) _ = False  -- Les commissariats ne peuvent pas ajouter de citoyens
prop_pre_construitBatiment (Cabane _ _ cap l) cid = cap > length l && notElem cid l
prop_pre_construitBatiment (Atelier _ _ cap l) cid = cap > length l && notElem cid l
prop_pre_construitBatiment (Epicerie _ _ cap l) cid = cap > length l && notElem cid l

-- Postcondition pour la fonction construitBatiment
prop_post_construitBatiment :: Batiment -> Batiment -> CitId -> Bool
prop_post_construitBatiment (Cabane f c cap l) (Cabane f' c' cap' l') cid =
    f == f' && c == c' && cap == cap' && cid `elem` l' && length l' == length l + 1
prop_post_construitBatiment (Atelier f c cap l) (Atelier f' c' cap' l') cid =
    f == f' && c == c' && cap == cap' && cid `elem` l' && length l' == length l + 1
prop_post_construitBatiment (Epicerie f c cap l) (Epicerie f' c' cap' l') cid =
    f == f' && c == c' && cap == cap' && cid `elem` l' && length l' == length l + 1
prop_post_construitBatiment _ _ _ = False  -- Pour les autres cas comme Commissariat ou lorsque les types ne correspondent pas
