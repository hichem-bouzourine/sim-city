{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Componnent.Citoyen where
import Componnent.Forme
import Componnent.Utils
import Componnent.Batiment (Batiment)

data Occupation = Travailler | Dormir | FaireCourses | Deplacer Batiment (Maybe [Coord]) deriving (Show, Eq)

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

-- Fonction de mise a jour de l'occupation
citoyenUpdateOccupation :: Citoyen -> Occupation -> Citoyen
citoyenUpdateOccupation (Immigrant coord etat _) occupation = Immigrant coord etat occupation
citoyenUpdateOccupation (Habitant coord etat batIds _) occupation = Habitant coord etat batIds occupation
citoyenUpdateOccupation (Emigrant coord _) occupation = Emigrant coord occupation

-- Getter de l'argent pour les citoyens
citoyenArgent :: Citoyen -> Int
citoyenArgent c = case citoyenEtat c of
    Just (x, _, _) -> x
    Nothing -> 0

-- Getter de la fatigue pour les citoyens
citoyenFatigue :: Citoyen -> Int
citoyenFatigue c = case citoyenEtat c of
    Just (_, x, _) -> x
    Nothing -> 0

-- Getter de la faim pour les citoyens
citoyenFaim :: Citoyen -> Int
citoyenFaim c = case citoyenEtat c of
    Just (_, _, x) -> x
    Nothing -> 0

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

-- Cette fonction verifie si un citoyen est apte a travailler   
estCitoyenTravailleur :: Citoyen -> Bool
estCitoyenTravailleur  (Habitant _ _ (_, _, _) _)= True
estCitoyenTravailleur _ = False

-- Cette fonction verifie si un citoyen est apte a faire des courses
estCitoyenFaireCourses :: Citoyen -> Bool
estCitoyenFaireCourses  (Habitant _ _ (_, _, _) _)= True
estCitoyenFaireCourses _ = False

-- Getter des batiments concernés pour un citoyen
citoyenBatiments :: Citoyen -> [BatId]
citoyenBatiments c = foldr includeIfJust [] [citoyenBatimentTravail c, citoyenBatimentCourse c, citoyenBatimentRepos c]
    where
    includeIfJust :: Maybe BatId -> [BatId] -> [BatId]
    includeIfJust (Just b) acc' = b : acc'
    includeIfJust _ acc' = acc'

-- Fonction pour mettre à jour la position d'un citoyen
updateCitoyenPosition :: Citoyen -> Coord -> Citoyen
updateCitoyenPosition (Habitant _ etat batIds occupation) coord = Habitant coord etat batIds occupation
updateCitoyenPosition (Immigrant _ etat occupation) coord = Immigrant coord etat occupation
updateCitoyenPosition (Emigrant _ occupation) coord = Emigrant coord occupation
updateCitoyenPosition c _ = c

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
affecteBatimentTravail' (Habitant coord etat (batId, _, b) _) batId' = Habitant coord etat (batId, Just batId', b) Travailler
affecteBatimentTravail' c _ = c
-- Précondition pour l'affectation d'un bâtiment de travail
prop_pre_affecteBatimentTravail' :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentTravail' (Habitant {}) _ = True
prop_pre_affecteBatimentTravail' _ _ = False

-- Postcondition pour l'affectation d'un bâtiment de travail
prop_post_affecteBatimentTravail' :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentTravail' (Habitant coord etat (mId, _, cId) occupation) 
                                    (Habitant coord' etat' (mId', tId', cId') occupation') batId =
    coord == coord' && etat == etat' && occupation == occupation'
    && mId == mId' && tId' == Just batId && cId == cId'

-- cette fonction permet d'assigner un batiment de course à un habitant
affecteBatimentCourse' :: Citoyen -> BatId -> Citoyen
affecteBatimentCourse' (Habitant coord etat (batId, _, _) _) batId' = Habitant coord etat (batId, Nothing, Just batId') FaireCourses
affecteBatimentCourse' c _ = c
-- Précondition pour l'affectation d'un bâtiment de course
prop_pre_affecteBatimentCourse' :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentCourse' (Habitant {}) _ = True
prop_pre_affecteBatimentCourse' _ _ = False

-- Postcondition pour l'affectation d'un bâtiment de course
prop_post_affecteBatimentCourse' :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentCourse' (Habitant coord etat (mId, _, cId) occupation) (Habitant coord' etat' (mId', tId', cId') occupation') batId =
    coord == coord' && etat == etat' && occupation == occupation'
    && mId == mId' && tId' == cId && cId' == Just batId

-- Cette fonction permet d'assigner un target de batiment a un citoyen
citoyenBatTarget :: Citoyen -> Batiment -> Citoyen
citoyenBatTarget (Habitant coord etat (mId, tId, cId) _) batiment = Habitant coord etat (mId, tId, cId) (Deplacer batiment Nothing)

-- Cette fonction permet de affecter un target de deplacement vers tout citoyen
citoyenFindTarget :: Citoyen -> Batiment -> [Coord] -> Citoyen
citoyenFindTarget (Habitant coord etat (mId, tId, cId) _) batiment path = Habitant coord etat (mId, tId, cId) (Deplacer batiment (Just path))
citoyenFindTarget (Immigrant coord etat _) batiment path = Immigrant coord etat (Deplacer batiment (Just path))
citoyenFindTarget (Emigrant coord _) batiment path = Emigrant coord (Deplacer batiment (Just path))

-- Mettre à jour l'argent d'un citoyen
citoyenUpdateArgent :: Citoyen -> Int -> Citoyen
citoyenUpdateArgent (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) occ) argent = Habitant coord (argent, etat2, etat3) (mId, tId, cId) occ 

-- Cette fonction permet de verifier si un citoyen est un Emigrant
estEmigrant :: Citoyen -> Bool
estEmigrant (Emigrant _ _) = True
estEmigrant _ = False
instance Show Citoyen where
    show ( Immigrant coord etat occupation ) =
        "Immigrant " ++ show coord ++ " " ++ show etat ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Nothing, Nothing) occupation) =
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Just batId2, Nothing) occupation) =
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show batId2 ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Just batId2, Just batId3) occupation) =
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show batId2 ++ " " ++ show batId3 ++ " " ++ show occupation
    show ( Habitant coord etat (batId, Nothing, Just bati) occupation) =
        "Habitant " ++ show coord ++ " " ++ show etat ++ " " ++ show batId ++ " " ++ show bati ++ " " ++ show occupation
    show ( Emigrant coord occupation ) = "Emigrant " ++ show coord ++ " " ++ show occupation
    

