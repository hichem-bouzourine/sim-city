{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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

-- Invariant pour vérifier l'état d'un citoyen
prop_inv_etatCitoyen :: Citoyen -> Bool
prop_inv_etatCitoyen citoyen = case citoyenEtat citoyen of
    Just (x, y, z) -> x >= 0 && y >= 0 && z >= 0
    Nothing -> True

-- fonction pour transformer un immigrant en habitant
integreVille :: Citoyen -> BatId -> Citoyen
integreVille (Immigrant coord etat occupation) batId = Habitant coord etat (batId, Nothing, Nothing) occupation
integreVille citoyen _ = citoyen  -- Si ce n'est pas un immigrant, pas de transformation

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
transformeEnEmigrant citoyen = citoyen  -- Si ce n'est pas un Habitant, retourne le citoyen inchangé

-- Précondition pour la transformation d'un Habitant en Emigrant
prop_pre_transformeEnEmigrant :: Citoyen -> Bool
prop_pre_transformeEnEmigrant (Habitant {}) = True
prop_pre_transformeEnEmigrant _ = False

-- Postcondition pour la transformation d'un Habitant en Emigrant
prop_post_transformeEnEmigrant :: Citoyen -> Citoyen -> Bool
prop_post_transformeEnEmigrant (Habitant coord _ _ occupation) (Emigrant coord' occupation') =
    coord == coord' && occupation == occupation'
prop_post_transformeEnEmigrant _ _ = False  -- La postcondition ne s'applique pas si l'entrée n'était pas un Habitant


-- instance Show pour citoyen
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
