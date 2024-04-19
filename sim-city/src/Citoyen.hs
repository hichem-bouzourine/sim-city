{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Citoyen where
import Forme
import Utils

-- import Data.Map (Map)
-- import qualified Data.Map as Map

data Occupation = Travailler | Dormir | FaireCourses | Deplacer Coord deriving (Show, Eq)

-- Type représentant un citoyen avec ses informations spécifiques
data Citoyen = Immigrant Coord (Int, Int, Int) Occupation
             | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation
             | Emigrant Coord Occupation

-- Instance d'égalité pour les citoyens
instance Eq Citoyen where
    (==) c1 c2 = citoyenCoord c1 == citoyenCoord c2
                 && citoyenEtat c1 == citoyenEtat c2
                 && citoyenOccupation c1 == citoyenOccupation c2

-- Getter de coordonnées pour les citoyens
citoyenCoord :: Citoyen -> Coord
citoyenCoord (Immigrant coord _ _) = coord
citoyenCoord (Habitant coord _ _ _) = coord
citoyenCoord (Emigrant coord _) = coord

-- Getter de l'occupation pour les citoyens
citoyenOccupation :: Citoyen -> Occupation
citoyenOccupation (Immigrant _ _ occupation) = occupation
citoyenOccupation (Habitant _ _ _ occupation) = occupation
citoyenOccupation (Emigrant _ occupation) = occupation

-- Getter de l'état pour les citoyens
citoyenEtat :: Citoyen -> Maybe (Int, Int, Int)
citoyenEtat (Immigrant _ etat _) = Just etat
citoyenEtat (Habitant _ etat _ _) = Just etat
citoyenEtat (Emigrant _ _) = Nothing  -- Les émigrants n'ont pas d'état défini

-- Getter de batiment de travail pour les citoyens
citoyenBatimentTravail :: Citoyen -> Maybe BatId
citoyenBatimentTravail (Habitant _ _ (_, tId, _) _) = tId
citoyenBatimentTravail _ = Nothing

-- Getter de batiment de course pour les citoyens
citoyenBatimentCourse :: Citoyen -> Maybe BatId
citoyenBatimentCourse (Habitant _ _ (_, _, cId) _) = cId
citoyenBatimentCourse _ = Nothing

-- Getter de batiment de repos pour les citoyens
citoyenBatimentRepos :: Citoyen -> Maybe BatId
citoyenBatimentRepos (Habitant _ _ (mId, _, _) _) = Just mId
citoyenBatimentRepos _ = Nothing

-- Invariant pour vérifier l'état d'un citoyen
prop_inv_etatCitoyen :: Citoyen -> Bool
prop_inv_etatCitoyen citoyen = case citoyenEtat citoyen of
    Just (x, y, z) -> x >= 0 && y >= 0 && z >= 0
    Nothing -> True

-- Invariant pour vérifier l'occupation d'un citoyen
prop_inv_occupationCitoyen :: Citoyen -> Bool
prop_inv_occupationCitoyen citoyen = case citoyenOccupation citoyen of
    Travailler -> case citoyen of
        Habitant _ _ (_, Just _, _) _ -> True
        _ -> False
    FaireCourses -> case citoyen of
        Habitant _ _ (_, _, Just _) _ -> True
        _ -> False
    Dormir -> case citoyen of
        Habitant _ _ (_, _, _) _ -> True
        _ -> False

-- Invariant pour vérifier un citoyen
prop_inv_citoyens :: Citoyen -> Bool
prop_inv_citoyens citoyen = 
    prop_inv_etatCitoyen citoyen
    && prop_inv_occupationCitoyen citoyen

-- Fonction pour transformer un immigrant en habitant
integreVille :: Citoyen -> BatId -> Citoyen
integreVille (Immigrant coord etat occupation) batId = Habitant coord etat (batId, Nothing, Nothing) occupation
integreVille _ _ = error "Impossible d'intégrer un citoyen qui n'est pas un immigrant"

-- Précondition pour l'intégration dans la ville
prop_pre_integreVille :: Citoyen -> BatId -> Bool
prop_pre_integreVille (Immigrant {}) _ = True
prop_pre_integreVille _ _ = False  -- Seuls les immigrants peuvent être intégrés

-- Postcondition pour l'intégration dans la ville
prop_post_integreVille :: Citoyen -> Citoyen -> Bool
prop_post_integreVille (Immigrant coord etat occupation) (Habitant coord' etat' _ occupation') =
    coord == coord' && etat == etat'
    && occupation == occupation'
prop_post_integreVille _ _ = False  -- La transformation doit respecter le changement de Immigrant à Habitant

-- Transformation d'un Habitant en Emigrant
transformeEnEmigrant :: Citoyen -> Citoyen
transformeEnEmigrant (Habitant coord _ _ occupation) = Emigrant coord occupation
transformeEnEmigrant _ = error "Impossible de transformer un citoyen qui n'est pas un habitant en émigrant"

-- Précondition pour la transformation d'un Habitant en Emigrant
prop_pre_transformeEnEmigrant :: Citoyen -> Bool
prop_pre_transformeEnEmigrant (Habitant {}) = True
prop_pre_transformeEnEmigrant _ = False

-- Postcondition pour la transformation d'un Habitant en Emigrant
prop_post_transformeEnEmigrant :: Citoyen -> Citoyen -> Bool
prop_post_transformeEnEmigrant (Habitant coord _ _ occupation) (Emigrant coord' occupation') =
    coord == coord' && occupation == occupation'
prop_post_transformeEnEmigrant _ _ = False  -- La postcondition ne s'applique pas si l'entrée n'était pas un Habitant

-- cette fonction permet d'assingner un batiment de travail à un habitant
affecteBatimentTravail' :: Citoyen -> BatId -> Citoyen
affecteBatimentTravail' (Habitant coord etat (batId, _, _) _) batId' = Habitant coord etat (batId, Just batId', Nothing) Travailler
affecteBatimentTravail' _ _ = error "Impossible d'affecter un bâtiment de travail à un citoyen qui n'est pas un habitant"

-- Précondition pour l'affectation d'un bâtiment de travail
prop_pre_affecteBatimentTravail' :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentTravail' (Habitant {}) _ = True
prop_pre_affecteBatimentTravail' _ _ = False

-- Postcondition pour l'affectation d'un bâtiment de travail
prop_post_affecteBatimentTravail' :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentTravail' (Habitant coord etat (mId, _, cId) occupation) (Habitant coord' etat' (mId', tId', cId') occupation') batId =
    coord == coord' && etat == etat' && occupation == occupation'
    && mId == mId' && tId' == Just batId && cId == cId'

-- cette fonction permet d'assigner un batiment de course à un habitant
affecteBatimentCourse' :: Citoyen -> BatId -> Citoyen
affecteBatimentCourse' (Habitant coord etat (batId, _, _) _) batId' = Habitant coord etat (batId, Nothing, Just batId') FaireCourses
affecteBatimentCourse' _ _ = error "Impossible d'affecter un bâtiment de course à un citoyen qui n'est pas un habitant"

-- Précondition pour l'affectation d'un bâtiment de course
prop_pre_affecteBatimentCourse' :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentCourse' (Habitant {}) _ = True
prop_pre_affecteBatimentCourse' _ _ = False

-- Postcondition pour l'affectation d'un bâtiment de course
prop_post_affecteBatimentCourse' :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentCourse' (Habitant coord etat (mId, _, cId) occupation) (Habitant coord' etat' (mId', tId', cId') occupation') batId =
    coord == coord' && etat == etat' && occupation == occupation'
    && mId == mId' && tId' == cId && cId' == Just batId

-- cette fonction permet met a jour l'etat(argent, fatigue, famine) d'un citoyen en fonction de son occupation
metAJourEtat :: Citoyen -> Citoyen
metAJourEtat (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Travailler) = 
    Habitant coord (etat1 + wGain, etat2 + wFaim, etat3 + wFatigue) (mId, tId, cId) Travailler
metAJourEtat (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) FaireCourses) =      
    Habitant coord (etat1, etat2 + cFaim, etat3) (mId, tId, cId) FaireCourses
metAJourEtat (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Dormir) =
    Habitant coord (etat1, etat2, etat3 + dFatigue) (mId, tId, cId) Dormir
metAJourEtat c = c

-- Précondition pour la mise à jour de l'état d'un citoyen
instance Show Citoyen where
    show ( Immigrant coord etat occupation ) = 
        "Immigrant " ++ show coord ++ " " ++ show etat ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Nothing, Nothing) occupation) = 
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Just batId2, Nothing) occupation) = 
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show batId2 ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Just batId2, Just batId3) occupation) = 
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show batId2 ++ " " ++ show batId3 ++ " " ++ show occupation
    show ( Emigrant coord occupation ) = "Emigrant " ++ show coord ++ " " ++ show occupation
    
