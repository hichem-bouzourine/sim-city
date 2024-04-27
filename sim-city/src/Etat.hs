{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Etat where


import Citoyen
import Environnement
import Utils
import Batiment
import Zone
import Ville
import qualified Data.Map as Map


data Etat = Etat {
    tourNB :: Int,
    environnement :: Environnement
}

--  Update the occupation of citizens based on their conditions
updateOccCitoyen :: Environnement -> Environnement
updateOccCitoyen env@(Env h w envBat (Ville zones citoyens) carte) = Env h w envBat (Ville zones (Map.map updateOccupation citoyens)) carte
  where
    updateOccupation :: Citoyen -> Citoyen
    updateOccupation c =
      case citoyenOccupation c of
        Travailler -> updateForWork c
        FaireCourses -> updateForShopping c
        Dormir -> updateForSleeping c
        Deplacer b -> if citoyenCoord c == batimentEntree b                       -- Si le citoyen est arrivé à destination
            then updateOccCitoyen' c (getBatIdFromBatiment b env)                       -- Il met à jour son ocupation
            else updateMove c $ batimentEntree b                                                   -- Sinon il continue de se déplacer
        _ -> c

    updateForWork c
        | citoyenFatigue c == 0 = case citoyenBatimentRepos c of                           -- Si le citoyen est fatigué il se dirige vers sa maison pour se reposer
            Just m -> citoyenBatTarget c (getBatimentWithId env m)
            _ -> c  -- a revoir
        | citoyenFaim c == 0 && citoyenArgent c > 0 = case citoyenBatimentCourse c of      -- Si le citoyen a faim il se dirige vers son batiment de course s'il dispose d'argent
                Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
                _ -> c -- a revoir                         
    updateForWork c = c                                                                    -- Autrement il continue de travailler

    updateForShopping c
        | citoyenFaim c < maxFaim && citoyenArgent c > 0 = c                               -- s'il n'ai pas rasasié et dispose d'argent il continue de faire les courses
        | otherwise = case citoyenBatimentRepos c of                                       -- s'il est rasasié il se dirige vers sa maison pour se reposer s'il en a
            Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
            _ -> case citoyenBatimentTravail c of                                          -- Autrement il se dirige vers son batiment de travail si possible
                Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
                _ ->  c -- a revoir

    updateForSleeping c
        | citoyenFatigue c < maxEnergie = c                                                -- il se repose tanqu'il n'ai pas en forme
        | otherwise = case citoyenBatimentTravail c of                                     -- Autrement il se dirige vers son batiment de travail si possible
            Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
            _ -> c -- a revoir

    updateOccCitoyen' c bId
        | bId == citoyenBatimentRepos c = citoyenUpdateOccupation c Dormir                  -- Si le citoyen est arrivé à sa maison il se repose
        | bId == citoyenBatimentCourse c = citoyenUpdateOccupation c FaireCourses           -- Si le citoyen est arrivé à son batiment de course il fait les courses
        | bId == citoyenBatimentTravail c = citoyenUpdateOccupation c Travailler            -- Si le citoyen est arrivé à son batiment de travail il travaille
        | otherwise = c -- Sinon il y reste (cas a voire)

    -- on va utuliser la carte pour trouver le plus cours chemin entre la position du citoyen et l'entree du batimet la ou il doit se deplacer
    -- retourne la coordener voisine du citoyen dans la direction du plus cours chemin
    -- updateMove :: Citoyen -> Coord -> Map Coord Char -> Citoyen
    updateMove c _ = c


    maxFaim = 10
    maxEnergie = 10


-- cette fonction permet met a jour l'etat(argent, fatigue, famine) d'un citoyen en fonction de son occupation
metAJourEtaCitoyen :: Citoyen -> Citoyen
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Travailler) =
    Habitant coord (etat1 + wGain, etat2 + wFaim, etat3 + wFatigue) (mId, tId, cId) Travailler
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) FaireCourses) =
    Habitant coord (etat1, etat2 + cFaim, etat3) (mId, tId, cId) FaireCourses
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Dormir) =
    Habitant coord (etat1, etat2, etat3 + dFatigue) (mId, tId, cId) Dormir
    -- il manque les autres type de citoyen a gerer 
metAJourEtaCitoyen c = c

-- Cette fonction permet de mettre a jour l'etat de tous les citoyens d'une ville


-- Fonction qui retire un citoyen d'une ville
retireCitoyen :: Environnement -> CitId -> Environnement
retireCitoyen (Env h w envBat (Ville zones citoyens) carte) cid =
    Env h w envBat (Ville ( Map.map removeCitoyen zones) (Map.delete cid citoyens)) newCarte
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

        -- Retire un citoyen d'un bâtiment
        newCarte = Map.insert (citoyenCoord (citoyens Map.! cid)) ' ' carte


-- Précondition pour la suppression d'un citoyen
prop_pre_retireCitoyen :: Environnement -> CitId -> Bool
prop_pre_retireCitoyen env cid =
    Map.lookup (citoyenCoord (villeGetCitoyen (envVille env) cid)) (envMap env) == Just 'x'  &&   -- Le citoyen doit exister et être sur la carte
    case envVille env of
        Ville _ citoyens -> Map.member cid citoyens


-- Postcondition pour la suppression d'un citoyen
prop_post_retireCitoyen :: Environnement -> Environnement -> CitId -> Bool
prop_post_retireCitoyen env env' cid =
        Map.lookup (citoyenCoord (villeGetCitoyen (envVille env) cid)) (envMap env) == Just ' '  &&   -- Le citoyen doit avoir été retiré de la carte

    case envVille env of
    Ville _ citoyens -> case envVille env' of
        Ville _ citoyens' -> Map.notMember cid citoyens' &&         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré
                     -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré         -- Le citoyen doit avoir été retiré
                     -- Le citoyen doit avoir été retiré
            Map.size citoyens' == Map.size citoyens - 1 &&          -- Il doit y avoir un citoyen de moins          -- Il doit y avoir un citoyen de moins
            notElem cid (villeBatimentsCitId (envVille env'))       -- Le citoyen ne doit plus être affecté à un bâtiment

-- Cette fonction permet de nettoyer un environement 
cleanEnv :: Environnement -> Environnement
cleanEnv env = foldr (flip retireCitoyen) env (villeCitIds (envVille env))

-- Affecte un bâtiment de travail à un habitant, si possible
affecteBatimentTravail :: Environnement -> BatId -> CitId -> Maybe Environnement
affecteBatimentTravail env@(Env h w envBat v@(Ville zones citoyens) carte) batId citId =
    let
        -- ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment =  construitBatiment (getBatimentWithId env batId) citId

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        newCitoyen =  affecteBatimentTravail' (villeGetCitoyen v citId) batId
    in
        case getZoneBatiment v newBatiment of
                Just (k, z) ->  Just  $ Env h w (Map.insert batId newBatiment envBat)                             -- mise a jour du batiment dans l'ennnuaire de l'enviroment 
                            (Ville (Map.insert k z zones ) (Map.insert citId newCitoyen citoyens)) carte             -- mise a jour du citoyen dans l'ennuaire de la ville
                _ -> Nothing

-- Précondition pour l'affectation d'un bâtiment de travail
prop_pre_affecteBatimentTravail :: Environnement  -> BatId -> CitId -> Bool
prop_pre_affecteBatimentTravail env@(Env _ _ _ (Ville _ citoyens) _) batId citId =
    Map.member citId citoyens &&
    let b = getBatimentWithId env batId in
    citId `notElem` batimentCitoyens b &&               -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté               -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
                   -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
    batimentCapacite b > length (batimentCitoyens b)    -- Le bâtiment doit avoir de la place

-- Postcondition pour l'affectation d'un bâtiment de travail
prop_post_affecteBatimentTravail :: Environnement -> Environnement -> BatId -> CitId -> Bool
prop_post_affecteBatimentTravail (Env _ _ _ (Ville zones _) _) (Env _ _ envBat (Ville zones' citoyens') _)  batId citId =
    length zones == length zones' &&                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même
                                     -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même
                                     -- Le nombre de zones doit être le même
    citId `elem` batimentCitoyens (envBat Map.! batId)&&             -- Le citoyen doit être affecté au bâtiment dans l'environnement             -- Le citoyen doit être affecté au bâtiment dans l'environnement
    case citoyenBatimentTravail (citoyens' Map.! citId) of           -- Le citoyen doit être affecté au bâtiment
        Just bId -> bId == batId
        _ -> False

-- Affecte un bâtiment de course à un habitant, si possible
affecteBatimentCourse :: Environnement -> BatId -> CitId -> Maybe Ville
affecteBatimentCourse (Env h w envBat v@(Ville zones citoyens) carte) batId citId =
    let
        -- Tentative d'ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment = construitBatiment (getBatimentWithId (Env h w envBat v carte) batId) citId

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        newCitoyen = affecteBatimentCourse' (villeGetCitoyen v citId) batId
    in
        case getZoneBatiment v newBatiment of
            Just (k, z) ->  Just  $ Ville (Map.insert k z zones ) (Map.insert citId newCitoyen citoyens)             -- mise a jour du citoyen dans l'ennuaire de la ville
            _ -> Nothing

-- Précondition pour l'affectation d'un bâtiment de course
prop_pre_affecteBatimentCourse :: Environnement -> BatId -> CitId -> Bool
prop_pre_affecteBatimentCourse a@(Env _ _ _ (Ville _ citoyens) _) batId citId =
    Map.member citId citoyens &&
    let b = getBatimentWithId a batId in
    citId `notElem` batimentCitoyens b &&               -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté               -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
                   -- Le bâtiment doit exister et le citoyen ne doit pas déjà être affecté
    batimentCapacite b > length (batimentCitoyens b)    -- Le bâtiment doit avoir de la place

-- Postcondition pour l'affectation d'un bâtiment de course
prop_post_affecteBatimentCourse :: Environnement -> Environnement -> BatId -> CitId -> Bool
prop_post_affecteBatimentCourse (Env _ _ _ (Ville zones _) _) (Env _ _ envBat (Ville zones' citoyens') _)  batId citId =
    length zones == length zones' &&                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même
                                     -- Le nombre de zones doit être le même                                 -- Le nombre de zones doit être le même
                                     -- Le nombre de zones doit être le même
    citId `elem` batimentCitoyens (envBat Map.! batId)&&             -- Le citoyen doit être affecté au bâtiment dans l'environnement             -- Le citoyen doit être affecté au bâtiment dans l'environnement
    case citoyenBatimentCourse (citoyens' Map.! citId) of           -- Le citoyen doit être affecté au bâtiment
        Just bId -> bId == batId
        _ -> False
