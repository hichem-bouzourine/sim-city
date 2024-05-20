{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Zone where
import Forme
import Batiment
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map


-- Définitions des zones et bâtiments
data Zone = Eau Forme
 | Route Forme
 | ZR Forme [Batiment]
 | ZI Forme [Batiment]
 | ZC Forme [Batiment]
 | Admin Forme Batiment

-- Getters de forme pour les Zones
zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f

-- Getters de Batiment pour les Zones
zoneBatiments :: Zone -> [Batiment]
zoneBatiments (ZR _ bs) = bs
zoneBatiments (ZI _ bs) = bs
zoneBatiments (ZC _ bs) = bs
zoneBatiments (Admin _ b) = [b]
zoneBatiments _ = []

-- Cette fonction determine la map de zone
zoneMap :: Zone -> Map Coord Char
zoneMap z = let zMap  = foldr (`Map.insert` '#') Map.empty (formeBordure $ zoneForme z)
               in foldr (Map.union . batimentMap) zMap (zoneBatiments z)
-- Constructeur 
-- Zone routier
-- on verifie si la forme est une ligne horizontale ou verticale
construitZoneRoute :: Forme -> Maybe Zone
construitZoneRoute f = case f of
    HSegement _ _ -> Just $ Route f         
    VSegement _ _ -> Just $ Route f
    _ -> Nothing

--  Zone routier
-- on verifie si la forme est une ligne horizontale ou verticale
construitZoneEau :: Forme -> Maybe Zone
construitZoneEau f = case f of
    HSegement _ _ -> Just $ Eau f           
    VSegement _ _ -> Just $ Eau f
    _ -> Nothing

-- Zone résidentielle
-- on verifie si la forme est un rectangle
construitZoneResidentielle :: Forme -> Maybe Zone
construitZoneResidentielle f = case f of
    Rectangle {} -> Just $ ZR f []
    _ -> Nothing

-- Zone industrielle
-- on verifie si la forme est un rectangle
construitZoneIndustrielle :: Forme -> Maybe Zone
construitZoneIndustrielle f = case f of
    Rectangle {} -> Just $ ZI f []
    _ -> Nothing

-- Zone commerciale
-- on verifie si la forme est un rectangle
construitZoneCommerciale :: Forme -> Maybe Zone
construitZoneCommerciale f = case f of
    Rectangle {} -> Just $ ZC f []
    _ -> Nothing

-- Zone administrative
-- on verifie si la forme est un rectangle
construitZoneAdmin :: Forme -> Batiment -> Maybe Zone
construitZoneAdmin f b = case f of
    Rectangle {} -> Just $ Admin f b
    _ -> Nothing

-- Invariant de forme pour les zone routiers 
prop_inv_ZoneRouteForme :: Zone -> Bool
prop_inv_ZoneRouteForme (Route f) = case f of
    HSegement _ _ -> True
    VSegement _ _ -> True
    _ -> False
prop_inv_ZoneRouteForme _ = True

-- Invariant de l'appartennace pour des batiments au zone
-- Les cabanes/ateliers/epiceries ne se trouvent que dans les zones r ́esidentielles/industrielles/commerciales.
prop_inv_zoneBatiment :: Zone -> Bool
prop_inv_zoneBatiment (Admin _ batiment) = case batiment of
    Cabane {} -> False
    Atelier {} -> False
    Epicerie {} -> False
    Commissariat _ _ -> True
prop_inv_zoneBatiment _ = True

-- Invariant : tout batiment dans une zone a une entrée adjacente à la forme de la zone
prop_inv_zoneBatimentAdj :: Zone -> Bool
prop_inv_zoneBatimentAdj z = all (\b -> adjacent (batimentEntree b) (zoneForme z)) (zoneBatiments z)

-- invariant pour vérifier que la forme de ses batiments se trouve dans la forme de la zone
prop_inv_zoneBatimentForme :: Zone -> Bool
prop_inv_zoneBatimentForme z = all (\b -> let (y1, y2, x1, x2) = limites (batimentForme b) in
    appartient (C x1 y1) (zoneForme z)
    && appartient (C x2 y2) (zoneForme z)
    && appartient (C x1 y2) (zoneForme z)
    && appartient (C x2 y1) (zoneForme z) ) (zoneBatiments z)

-- Invariant de zone
prop_inv_Zone :: Zone -> Bool
prop_inv_Zone z = prop_inv_ZoneRouteForme z && prop_inv_zoneBatiment z && prop_inv_zoneBatimentAdj z && prop_inv_zoneBatimentForme z

-- Identifiants pour les éléments de la ville
newtype ZoneId = ZoneId Int deriving (Eq, Ord)

-- Construit une zone en ajoutant un bâtiment
-- si le batiment existe deja on le met juste a jour vu qu'on update pas la forme
-- si le batiment n'existe pas on verifie si ils n'ai pas en collision avec les autres batiments
construitZone :: Zone -> Batiment -> Maybe Zone
construitZone zone b
    | not $ contenue (batimentForme b) (zoneForme zone) = Nothing  -- La forme du bâtiment n'est pas contenue dans la zone
    | b `elem` zoneBatiments zone = Just $ updateZoneBtiment zone b     -- Le bâtiment existe déjà, on le met à jour
    | any (collision' (batimentForme b) . batimentForme) (zoneBatiments zone) = Nothing  -- Il y a une collision
    |
    --  adjacentes (batimentForme b) (zoneForme zone) &&                           -- Le bâtiment est adjacent à la zone                            
                                   -- Le bâtiment est adjacent à la zone 
        batimentEntree b `adjacent` zoneForme zone  &&     -- et l'entree doit etre aussi adjacente    
        -- Les cabanes/ateliers/epiceries ne se trouvent que dans les zones r ́esidentielles/industrielles/commerciales.
        case zone of
            Route _ -> False                    -- impossible de construire dans une route
            Eau _ -> False                      -- impossible de construire dans l'eau
            Admin _ bat -> case bat of
                Commissariat _ _ -> True        -- seul un commissariat peut etre construit dans une zone admin
                _ -> False
            _ -> True
    = Just $ addBatimentToZone b zone
    | otherwise = Nothing -- on serais dans un cas non gerer

-- Ajoute un bâtiment à une zone
addBatimentToZone :: Batiment -> Zone -> Zone
addBatimentToZone b (ZR f bs) = ZR f (b:bs)
addBatimentToZone b (ZI f bs) = ZI f (b:bs)
addBatimentToZone b (ZC f bs) = ZC f (b:bs)
addBatimentToZone b (Admin f _) = Admin f b
addBatimentToZone _ zone = zone

instance Eq Zone where
    (==) (Eau f) (Eau f') = f == f'
    (==) (Route f) (Route f') = f == f'
    (==) (ZR f bs) (ZR f' bs') = f == f' && bs == bs'
    (==) (ZI f bs) (ZI f' bs') = f == f' && bs == bs'
    (==) (ZC f bs) (ZC f' bs') = f == f' && bs == bs'
    (==) (Admin f b) (Admin f' b') = f == f' && b == b'
    (==) _ _ = False

-- preconditions pour la fonction construitZone
prop_pre_construitZone :: Zone -> Batiment -> Bool
prop_pre_construitZone (Eau _) _ = False
prop_pre_construitZone (Route _) _ = False
prop_pre_construitZone  z b = b `notElem` zoneBatiments z && batimentEntree b `adjacent` zoneForme z

-- PostConditions pour la fonction construitZone
prop_post_construitZone :: Zone -> Zone -> Batiment -> Bool
prop_post_construitZone (Eau _) _ _ = False
prop_post_construitZone (Route _) _ _ = False
prop_post_construitZone (Admin f _) (Admin f' b') b = f == f' && b == b'
prop_post_construitZone  z1 z2 b = zoneForme z1 == zoneForme z2
    && b `elem` zoneBatiments z2
    && length (zoneBatiments z2) == length (zoneBatiments z1) + 1
                                                                 -- le batiment doit etre adjacent à la forme de la zone ?
                                                                 -- oubien on ne considere pas les invariants les proce
-- Retire un bâtiment d'une zone
retireBatiment :: Zone -> Batiment -> Zone
retireBatiment (ZR f bs) b = ZR f (filter (/= b) bs)
retireBatiment (ZI f bs) b = ZI f (filter (/= b) bs)
retireBatiment (ZC f bs) b = ZC f (filter (/= b) bs)
retireBatiment zone _ = zone  -- Pour les autres types de zones, on ne modifie pas les bâtiments

-- Précondition pour la fonction de suppression d'un bâtiment
prop_pre_retireBatiment :: Zone -> Batiment -> Bool
prop_pre_retireBatiment zone batiment = batiment `elem` zoneBatiments zone

-- Postcondition pour la fonction de suppression d'un bâtiment
prop_post_retireBatiment :: Zone -> Zone -> Batiment -> Bool
prop_post_retireBatiment zone nouvelleZone batiment =
    notElem batiment (zoneBatiments nouvelleZone) &&  -- Le bâtiment ne doit plus être présent     -- Le bâtiment ne doit plus être présent     -- Le bâtiment ne doit plus être présent     -- Le bâtiment ne doit plus être présent   
      -- Le bâtiment ne doit plus être présent   
    length (zoneBatiments zone) == length (zoneBatiments nouvelleZone) + 1  -- Un bâtiment doit avoir été retiré

-- Mise a jour d'un bâtiment d'une zone
updateZoneBtiment :: Zone -> Batiment -> Zone
updateZoneBtiment (Admin f _) b = Admin f b
updateZoneBtiment (ZR f bs) b = ZR f (map (\b' -> if b' == b then b else b') bs)
updateZoneBtiment (ZI f bs) b = ZI f (map (\b' -> if b' == b then b else b') bs)
updateZoneBtiment (ZC f bs) b = ZC f (map (\b' -> if b' == b then b else b') bs)
updateZoneBtiment zone _ = zone  -- Pour les autres types de zones, on ne modifie pas les bâtiments

-- Précondition pour la fonction de mise à jour d'un bâtiment
prop_pre_updateZoneBtiment :: Zone -> Batiment -> Bool
prop_pre_updateZoneBtiment (Eau _) _ = False
prop_pre_updateZoneBtiment (Route _) _ = False
prop_pre_updateZoneBtiment z b = b `elem` zoneBatiments z

-- Postcondition pour la fonction de mise à jour d'un bâtiment
prop_post_updateZoneBtiment :: Zone -> Zone -> Batiment -> Bool
prop_post_updateZoneBtiment (Eau _) _ _ = False
prop_post_updateZoneBtiment (Route _) _ _ = False
prop_post_updateZoneBtiment (Admin f _) (Admin f' b') b = f == f' && b == b'
prop_post_updateZoneBtiment z1 z2 b = zoneForme z1 == zoneForme z2
    && b `elem` zoneBatiments z2
    && length (zoneBatiments z2) == length (zoneBatiments z1)


-- Cette fonction verifie si un zonne est conforme : sans collision
-- on verifie pas les colision entre un batiment et lui meme
zoneConforme :: Zone -> Bool
zoneConforme z = let bs = zoneBatiments z
                     in all (\b -> all (\b' -> b == b' || not (collision' (batimentForme b) (batimentForme b'))) bs) bs

-- instancer show pour les zones
instance Show Zone where
    show (Eau f) = "Eau " ++ show f
    show (Route f) = "Route " ++ show f
    show (ZR f bs) = "ZR " ++ show f ++ " " ++ show bs
    show (ZI f bs) = "ZI " ++ show f ++ " " ++ show bs
    show (ZC f bs) = "ZC " ++ show f ++ " " ++ show bs
    show (Admin f b) = "Admin " ++ show f ++ " " ++ show b
