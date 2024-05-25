{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Componnent.Etat where

import Componnent.Citoyen
import Componnent.Environnement
import Componnent.Utils
import Componnent.Batiment
import Componnent.Zone
import Componnent.Ville
import Componnent.Forme

import qualified Data.Map as Map
import Data.Map (Map)
import Keyboard (Keyboard)
import Componnent.Graph (aStar)

data Etat = Etat {
    tourNB :: Int,
    environnement :: Environnement,
    coin :: Int,
    selected :: Maybe Keyboard,
    lastMousePosition :: Maybe Coord 
}

-- Cette fonction permet de d'initialiser un Etat 
initEtat :: Int -> Int -> Int -> Etat
initEtat coin w h = Etat 0 (initEnv h w) coin Nothing Nothing

-- Cette fonction permet de mettre a jour selected
updateSelected :: Etat -> Maybe Keyboard -> Etat
updateSelected (Etat n env coin _ lmp) kbd = Etat n env coin kbd lmp

updateLastMousePosition :: Etat -> Maybe Coord -> Etat
updateLastMousePosition (Etat n env coin kbd _) lmp = Etat n env coin kbd lmp

-- Cette fonction permet d'ajouter un immigrant dans l'environnement
addImmigrant :: Int -> Coord -> Etat -> Etat
addImmigrant id c e@(Etat n env coin _ lmp) = 
    let cityoen = case (getCommissariats env, isRoadPoint env c) of
                    (Just (_, com), True) -> Just $ Immigrant c (50, 1000, 1000) $ Deplacer com Nothing
                    _ -> Nothing
    in case cityoen of
        -- Si on a une voie ou débarquer l'immigrant et un commissariat pour l'intégrer
        -- on l'ajoute et il se dirige vers
        Just c -> Etat n (envAddCitoyen (CitId $ show id) c env) coin Nothing lmp
        _ -> e


-- Cette fonction permet de representer un Etat
showEtat :: Etat -> [String]
showEtat etat = 
    [ "NB Poppulation: " ++ show (Map.size $ viCit (envVille (environnement etat)))
    , "NB de chomage: " ++ show (tauxChomage (envVille (environnement etat)))
    , "NB client epicerie: " ++ show (nombreCitoyensCourses (eville (environnement etat)))
    , "NB Zone: " ++ show (Map.size (viZones (eville (environnement etat))))
    , "NB Batiments: " ++ show (Map.size (envBatiments (environnement etat)))
    , "Coin: " ++ show (coin etat)
    , "Selected: " ++ show (selected etat)
    , "Tour: " ++ show (tourNB etat)
    ]

--  Update the occupation of citizens based on their conditions
updateOccCitoyens :: Environnement -> Environnement
updateOccCitoyens env@(Env h w envBat v@(Ville zones citoyens) carte) = Env h w envBat (Ville zones (Map.map updateOccupation citoyens)) carte
  where
    updateOccupation :: Citoyen -> Citoyen
    updateOccupation c =
      case citoyenOccupation c of
        Travailler -> updateForWork c
        FaireCourses -> updateForShopping c
        Dormir -> updateForSleeping c
        Deplacer b p -> if citoyenCoord c == batimentEntree b                             -- Si le citoyen est arrivé à destination
            then updateOccCitoyen' c (getBatIdFromBatiment b env)                       -- Il met à jour son ocupation
            else updateMove c b p                                                  -- Sinon il continue de se déplacer
        _ -> c
        
    updateForWork c
        | citoyenFatigue c <= 0 = case citoyenBatimentRepos c of                           -- Si le citoyen est fatigué il se dirige vers sa maison pour se reposer
            Just m -> citoyenBatTarget c (getBatimentWithId env m)
            _ -> c  
        | citoyenFaim c <= 0 && citoyenArgent c > 0 = case citoyenBatimentCourse c of      -- Si le citoyen a faim il se dirige vers son batiment de course s'il dispose d'argent
                Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
                _ -> c                          
    updateForWork c = c                                                                    -- Autrement il continue de travailler

    updateForShopping c
        | citoyenFaim c < maxFaim && citoyenArgent c > 0 = c                               -- s'il n'ai pas rasasié et dispose d'argent il continue de faire les courses
        | otherwise = case citoyenBatimentRepos c of                                       -- s'il est rasasié il se dirige vers sa maison pour se reposer s'il en a
            Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
            _ -> case citoyenBatimentTravail c of                                          -- Autrement il se dirige vers son batiment de travail si possible
                Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
                _ ->  c 

    updateForSleeping c
        | citoyenFatigue c < maxEnergie = c                                                -- il se repose tanqu'il n'ai pas en forme
        | otherwise = case citoyenBatimentTravail c of                                     -- Autrement il se dirige vers son batiment de travail si possible
            Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
            _ -> case citoyenBatimentCourse c of                                           -- Autrement il se dirige vers son batiment de course si possible
                Just ident -> citoyenBatTarget c (getBatimentWithId env ident)
                _ -> c

    updateOccCitoyen' c bId
        | bId == citoyenBatimentRepos c = citoyenUpdateOccupation c Dormir                  -- Si le citoyen est arrivé à sa maison il se repose
        | bId == citoyenBatimentCourse c = citoyenUpdateOccupation c FaireCourses           -- Si le citoyen est arrivé à son batiment de course il fait les courses
        | bId == citoyenBatimentTravail c = citoyenUpdateOccupation c Travailler            -- Si le citoyen est arrivé à son batiment de travail il travaille
        | otherwise = c 

    -- on va utuliser la carte pour trouver le plus cours chemin entre la position du citoyen et l'entree du batimet la ou il doit se deplacer
    -- retourne la coordener voisine du citoyen dans la direction du plus cours chemin
    -- updateMove :: Citoyen -> Coord -> Map Coord Char -> Citoyen
    updateMove :: Citoyen -> Batiment -> Maybe [Coord] -> Citoyen
    updateMove c b p = case p of 
                        (Just (x:xs)) -> updateCitoyenPosition (citoyenFindTarget c b xs) x
                        _-> case  aStar (initMap v) (citoyenCoord c) (batimentEntree b) 50 of 
                            Just a -> updateCitoyenPosition (citoyenFindTarget c b $ tail (reverse a) ++ [batimentEntree b]) $ last a
                            _ -> c
                        _ -> c
maxFaim = 100
maxEnergie = 100


-- cette fonction permet met a jour l'etat(argent, fatigue, famine) d'un citoyen en fonction de son occupation
metAJourEtaCitoyen :: Citoyen -> Citoyen
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Travailler) =
    Habitant coord (etat1 + wGain, etat2 + wFaim, etat3 + wFatigue) (mId, tId, cId) Travailler
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) FaireCourses) =
    Habitant coord (etat1, etat2, etat3 + cFaim) (mId, tId, cId) FaireCourses
metAJourEtaCitoyen (Habitant coord (etat1, etat2, etat3) (mId, tId, cId) Dormir) =
    Habitant coord (etat1, etat2 + dFatigue, etat3) (mId, tId, cId) Dormir
    -- il manque les autres type de citoyen a gerer 
metAJourEtaCitoyen (Immigrant coord (etat1, etat2, etat3) occupation) = 
    Immigrant coord (etat1 + wGain, etat2 + wFaim, etat3 + wFatigue) occupation
metAJourEtaCitoyen c = c

-- Cette fonction permet de prelever des impots sur un facteur de temps donnée qui est ici 1217
-- elle preleve 10% de l'argent de chaque citoyen et les rajoute au coin de l'etat
-- elle retourne l'environnement mis a jour
updateImpots :: Etat -> Etat
updateImpots e@(Etat n env coin s c) = 
    -- si le tour n est un multipl 1217 
    if n `mod` 1217 /= 0 then e else
        Etat n env' (coin + impots) s c
        where
            impots = Map.foldr (\citoyen acc -> let a = citoyenArgent(citoyen) in acc + (a `div` 10)) 0 citoyens
            citoyens' = Map.map citoyenPreleveImpots citoyens
            Env h w envBat (Ville zones citoyens) carte = env
            env' = Env h w envBat (Ville zones citoyens') carte

            citoyenPreleveImpots :: Citoyen -> Citoyen
            citoyenPreleveImpots citoyen = let a = citoyenArgent citoyen in citoyenUpdateArgent citoyen (a - (a `div` 10))

-- Cette fonction permet d'effectuer la transistion administrative
-- Si un Immigration est a arriver a la porte d'entré du batiment ou il se deplaçait 
-- on chercher cherche s'il exitste une maison de disponible dans la liste des batiments de repos
-- si oui on le transform en Habitant avec comme batiments de repos la maison trouvé
-- sinon on le transform en Emigrant 
updateAdminstration :: Environnement -> Environnement
updateAdminstration env@(Env h w envBat (Ville zones citoyens) carte) = 
    let
        citoyens' = Map.map transitionAdministrative' citoyens
    in
        Env h w envBat (Ville zones citoyens') carte
    where
        transitionAdministrative' :: Citoyen -> Citoyen
        transitionAdministrative' c = case citoyenOccupation c of
            Deplacer b _ -> if citoyenCoord c == batimentEntree b then
                case (getBatimentRepos env, c)  of
                    (Just (bid, b), (Immigrant _ _ _))  -> Habitant (citoyenCoord c) (50, 0, 0) (bid, Nothing, Nothing) $ Deplacer b Nothing
                    (_, (Habitant _ _ _ _)) -> c
                    (_, (Immigrant _ _ _)) -> Emigrant (citoyenCoord c) Dormir
                    _ -> c
                else c
            _ -> c
            
            
-- Cette fonction permet de mettre d'assigner des batiments au citoyens
-- elle recupere la liste de tout les batiments de travaille libre et les affecte au citoyns qui n'on pas de batiment de travaille
-- elle recupere la liste de tout les batiments de course libre et les affecte au citoyns qui n'on pas de batiment de course
-- elle retourne l'environnement mis a jour
affecteBatiment :: Environnement -> Environnement
affecteBatiment env@(Env _ _ _ (Ville _ citoyens) _) = 
    let citoyensSansTravail = Map.filter (\c -> estCitoyenTravailleur c && case citoyenBatimentTravail c of
            Just _ -> False
            _ -> True) citoyens
        citoyensSansCourse = Map.filter (\c -> estCitoyenFaireCourses c && case citoyenBatimentCourse c of
            Just _ -> False
            _ -> True) citoyens
        env' = foldr (\cid acc -> case getBatimentTravail env of
            Just (bid, _) -> affecteBatimentTravail acc bid cid
            _ -> acc) env (Map.keys citoyensSansTravail)
    in foldr (\cid acc -> case getBatimentCourse env of
        Just (bid, _) -> affecteBatimentCourse acc bid cid
        _ -> acc) env' (Map.keys citoyensSansCourse)
        
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

-- Cette fonction permet de nettoyer un environement en supprimant tous les citoyens qui on etat en desous de 0
cleanEnv :: Environnement -> Environnement
cleanEnv env = Map.foldrWithKey aux env (viCit (envVille env))
    where
        aux :: CitId -> Citoyen -> Environnement -> Environnement
        aux cId citoyen acc = 
            if estEmigrant citoyen
                then retireCitoyen acc cId else acc
            
            
-- Affecte un bâtiment de travail à un habitant, si possible
affecteBatimentTravail :: Environnement -> BatId -> CitId -> Environnement
affecteBatimentTravail env@(Env h w envBat v@(Ville zones citoyens) carte) batId citId =
    let
        -- ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment =  construitBatiment (getBatimentWithId env batId) citId

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        newCitoyen =  affecteBatimentTravail' (villeGetCitoyen v citId) batId
    in
        case getZoneBatiment v newBatiment of
                Just (k, z) -> Env h w (Map.insert batId newBatiment envBat)                             -- mise a jour du batiment dans l'ennnuaire de l'enviroment 
                            (Ville (Map.insert k z zones ) (Map.insert citId newCitoyen citoyens)) carte             -- mise a jour du citoyen dans l'ennuaire de la ville
                _ -> error "Erreur d'affectation du batiment"

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
affecteBatimentCourse :: Environnement -> BatId -> CitId -> Environnement
affecteBatimentCourse (Env h w envBat v@(Ville zones citoyens) carte) batId citId =
    let
        -- Tentative d'ajout du citoyen au bâtiment spécifié dans la zone spécifiée
        newBatiment = construitBatiment (getBatimentWithId (Env h w envBat v carte) batId) citId

        -- Mise à jour le citoyen avec le nouveau bâtiment de travail
        newCitoyen = affecteBatimentCourse' (villeGetCitoyen v citId) batId
    in
        case getZoneBatiment v newBatiment of
            Just (k, z) ->  Env h w (Map.insert batId newBatiment envBat)                             -- mise a jour du batiment dans l'ennnuaire de l'enviroment 
                            (Ville (Map.insert k z zones ) (Map.insert citId newCitoyen citoyens)) carte             -- mise a jour du citoyen dans l'ennuaire de la ville
            _ -> error "Erreur d'affectation du batiment"

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