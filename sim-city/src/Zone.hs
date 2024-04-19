{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Zone where
import Forme
import Batiment

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
construitZone :: Zone -> Batiment -> Zone
construitZone (Admin f _) b = Admin f b
construitZone (ZR f bs) b = ZR f (b:bs)
construitZone (ZI f bs) b = ZI f (b:bs)
construitZone (ZC f bs) b = ZC f (b:bs)
construitZone _ _ = error "Impossible de construire un bâtiment dans cette zone: "

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
    notElem batiment (zoneBatiments nouvelleZone) &&  -- Le bâtiment ne doit plus être présent
    length (zoneBatiments zone) == length (zoneBatiments nouvelleZone) + 1  -- Un bâtiment doit avoir été retiré

-- instancer show pour les zones
instance Show Zone where
    show (Eau f) = "Eau " ++ show f
    show (Route f) = "Route " ++ show f
    show (ZR f bs) = "ZR " ++ show f ++ " " ++ show bs
    show (ZI f bs) = "ZI " ++ show f ++ " " ++ show bs
    show (ZC f bs) = "ZC " ++ show f ++ " " ++ show bs
    show (Admin f b) = "Admin " ++ show f ++ " " ++ show b
