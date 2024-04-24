module Etat where

import Citoyen
import Environement
import Forme
import Utils
import Batiment
import Zone
import Ville

import Data.Map (Map)
import qualified Data.Map as Map


-- | Update the occupation of citizens based on their conditions
-- metAJourOccCitoyen :: Environement -> Environement
-- metAJourOccCitoyen (Env h w envBat (Ville zones citoyens)) = Env h w envBat $ Ville zones (Map.map updateOccupation citoyens)
--   where
--     updateOccupation :: Citoyen -> Citoyen
--     updateOccupation c@(Habitant coord (etat1, etat2, etat3) (mId, tId, cId) occupation) =
--       case occupation of
--         Travailler -> updateForWork c etat2 etat3
--         FaireCourses -> updateForShopping c etat2 etat3
--         Dormir -> updateForSleeping c etat2 etat3
--         _ -> c  -- No change for Deplacer or other states

--     updateForWork c etat2 etat3
--       | etat2 <= 5 = moveToBuilding c mId
--       | etat3 <= 5 = maybe c (moveToBuilding c) cId
--       | otherwise = c

--     updateForShopping c etat2 etat3 
--       | etat3 >= maxFaim && etat2 < maxEnergie = moveToBuilding c mId
--       | etat3 >= maxFaim = maybe c (moveToBuilding c) tId
--       | otherwise = c

--     updateForSleeping c etat2 etat3 =
--       if etat2 >= maxEnergie && etat3 < maxFaim then maybe c (moveToBuilding c) cId
--       else c

--     moveToBuilding :: Citoyen -> Citoyen 
--     moveToBuilding c = undefined
        

    -- getBatimentWithId :: Map BatId Batiment -> BatId -> Maybe Batiment
    -- getBatimentWithId batiments bid = Map.lookup bid batiments

    -- maxFaim = 10
    -- maxEnergie = 10


-- cette fonction permet met a jour l'etat(argent, fatigue, famine) d'un citoyen en fonction de son occupation
metAJourEtaCitoyen :: Citoyen -> Citoyen
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Travailler) = 
    Habitant coord (etat1 + wGain, etat2 + wFaim, etat3 + wFatigue) (mId, tId, cId) Travailler
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) FaireCourses) =      
    Habitant coord (etat1, etat2 + cFaim, etat3) (mId, tId, cId) FaireCourses
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Dormir) =
    Habitant coord (etat1, etat2, etat3 + dFatigue) (mId, tId, cId) Dormir
metAJourEtaCitoyen c = c

-- Cette fonction permet de mettre a jour l'etat de tous les citoyens d'une ville


-- Fonction qui retire un citoyen d'une ville
retireCitoyen :: Environement -> CitId -> Environement
retireCitoyen (Env h w envBat (Ville zones citoyens)) cid = Env h w envBat $ Ville ( Map.map removeCitoyen zones) (Map.delete cid citoyens)
    -- Retire le citoyen de tous les bâtiments où il est affecté
    where
        removeCitoyen :: Zone -> Zone
        removeCitoyen z = case z of
            Admin f b -> Admin f (supprimeCitoyenBatiment b cid)
            ZR f bs -> ZR f (map (supprimeCitoyenBatiment' cid) bs)
            ZI f bs -> ZI f (map (supprimeCitoyenBatiment' cid) bs)
            ZC f bs -> ZC f (map (supprimeCitoyenBatiment' cid) bs)
            _ -> z

        supprimeCitoyenBatiment' :: CitId -> Batiment -> Batiment
        supprimeCitoyenBatiment' cid b = supprimeCitoyenBatiment b cid

-- Précondition pour la suppression d'un citoyen
prop_pre_retireCitoyen :: Environement -> CitId -> Bool
prop_pre_retireCitoyen env cid = case envVille env of
    Ville _ citoyens -> Map.member cid citoyens
    
-- Postcondition pour la suppression d'un citoyen
prop_post_retireCitoyen :: Environement -> Environement -> CitId -> Bool
prop_post_retireCitoyen env env' cid = case envVille env of
    Ville _ citoyens -> case envVille env' of
        Ville _ citoyens' -> Map.notMember cid citoyens' &&         -- Le citoyen doit avoir été retiré
            Map.size citoyens' == Map.size citoyens - 1 &&          -- Il doit y avoir un citoyen de moins
            notElem cid (villeBatimentsCitId (envVille env'))       -- Le citoyen ne doit plus être affecté à un bâtiment

-- Affecte un bâtiment de travail à un habitant, si possible
affecteBatimentTravail :: Environement -> BatId -> CitId -> Maybe Environement
affecteBatimentTravail env@(Env h w envBat v@(Ville zones citoyens)) batId citId =
    let
        -- Tentative d'ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment = case getBatimentWithId env batId of
            Just batiment -> Just $ construitBatiment batiment citId
            _ -> Nothing

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        nouveauCitoyen = case Map.lookup citId citoyens of
            Just citoyen -> Just $ affecteBatimentTravail' citoyen batId
            _ -> Nothing
    in
        case (newBatiment, nouveauCitoyen) of
            (Just b, Just c) -> case getZoneBatiment v b of
                Just (k, z) ->  Just  $ Env h w (Map.insert batId b envBat)                             -- mise a jour du batiment dans l'ennnuaire de l'enviroment 
                            $ Ville (Map.insert k z zones ) (Map.insert citId c citoyens)             -- mise a jour du citoyen dans l'ennuaire de la ville
                _ -> Nothing
            _ -> Nothing

-- Précondition pour l'affectation d'un bâtiment de travail
prop_pre_affecteBatimentTravail :: Environement  -> BatId -> CitId -> Bool
prop_pre_affecteBatimentTravail a@(Env _ _ _ (Ville _ citoyens)) batId citId =
    Map.member citId citoyens &&
    case getBatimentWithId a batId of 
        Just b -> citId `notElem` batimentCitoyens b &&         -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
            batimentCapacite b > length (batimentCitoyens b)    -- Le bâtiment doit avoir de la place
        _ -> False 

-- Postcondition pour l'affectation d'un bâtiment de travail
prop_post_affecteBatimentTravail :: Environement -> Environement -> BatId -> CitId -> Bool
prop_post_affecteBatimentTravail (Env _ _ _ (Ville zones _)) (Env _ _ envBat (Ville zones' citoyens'))  batId citId =
    length zones == length zones' &&                                 -- Le nombre de zones doit être le même
    citId `elem` batimentCitoyens (envBat Map.! batId)&&             -- Le citoyen doit être affecté au bâtiment dans l'environnement
    case citoyenBatimentTravail (citoyens' Map.! citId) of           -- Le citoyen doit être affecté au bâtiment
        Just bId -> bId == batId
        _ -> False

-- Affecte un bâtiment de course à un habitant, si possible
affecteBatimentCourse :: Environement -> BatId -> CitId -> Maybe Ville
affecteBatimentCourse (Env h w envBat v@(Ville zones citoyens)) batId citId =
    let
        -- Tentative d'ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment = case getBatimentWithId (Env h w envBat (Ville zones citoyens)) batId of
            Just batiment -> Just $ construitBatiment batiment citId
            _ -> Nothing

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        nouveauCitoyen = case Map.lookup citId citoyens of
            Just citoyen -> Just $ affecteBatimentCourse' citoyen batId
            _ -> Nothing
    in
        case (newBatiment, nouveauCitoyen) of
            (Just b, Just c) -> case getZoneBatiment v b of
                Just (k, z) ->  Just  $ Ville (Map.insert k z zones ) (Map.insert citId c citoyens)             -- mise a jour du citoyen dans l'ennuaire de la ville
                _ -> Nothing
            _ -> Nothing

-- Précondition pour l'affectation d'un bâtiment de course
prop_pre_affecteBatimentCourse :: Environement -> BatId -> CitId -> Bool
prop_pre_affecteBatimentCourse a@(Env _ _ _ (Ville _ citoyens)) batId citId =
    Map.member citId citoyens &&
    case getBatimentWithId a batId of 
        Just b -> citId `notElem` batimentCitoyens b &&         -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
            batimentCapacite b > length (batimentCitoyens b)    -- Le bâtiment doit avoir de la place
        _ -> False

-- Postcondition pour l'affectation d'un bâtiment de course
prop_post_affecteBatimentCourse :: Environement -> Environement -> BatId -> CitId -> Bool
prop_post_affecteBatimentCourse (Env _ _ _ (Ville zones _)) (Env _ _ envBat (Ville zones' citoyens'))  batId citId =
    length zones == length zones' &&                                 -- Le nombre de zones doit être le même
    citId `elem` batimentCitoyens (envBat Map.! batId)&&             -- Le citoyen doit être affecté au bâtiment dans l'environnement
    case citoyenBatimentCourse (citoyens' Map.! citId) of           -- Le citoyen doit être affecté au bâtiment
        Just bId -> bId == batId
        _ -> False


