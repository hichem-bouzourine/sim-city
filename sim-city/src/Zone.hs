{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Zone where
import Forme
import Batiment
import Data.Map (Map)
import qualified Data.Map as Map

-- Définitions des zones et bâtiments
data Zone = Eau Forme
 | Route Forme
 | ZR Forme [Batiment]
 | ZI Forme [Batiment]
 | ZC Forme [Batiment]
 | Admin Forme Batiment

zoneForme :: Zone -> Forme
zoneForme (Eau f) = f
zoneForme (Route f) = f
zoneForme (ZR f _) = f
zoneForme (ZI f _) = f
zoneForme (ZC f _) = f
zoneForme (Admin f _) = f

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

-- Invariant de zone
prop_inv_Zone :: Zone -> Bool
prop_inv_Zone z = prop_inv_ZoneRouteForme z && prop_inv_zoneBatiment z

-- Identifiants pour les éléments de la ville
newtype ZoneId = ZoneId Int deriving (Eq, Ord)

-- Construit une zone en ajoutant un bâtiment
construitZone :: Zone -> Batiment -> Zone
construitZone (ZR f bs) b = ZR f (b:bs)
construitZone (ZI f bs) b = ZI f (b:bs)
construitZone (ZC f bs) b = ZC f (b:bs)
construitZone (Admin f _) b = Admin f b
construitZone _ _ = error "Impossible de construire un bâtiment cette zone: "

-- preconditions pour la fonction construitZone
prop_pre_construitZone :: Zone -> Batiment -> Bool
prop_pre_construitZone (Eau _) _ = False
prop_pre_construitZone (Route _) _ = False
prop_pre_construitZone  (ZR _ bs) b = b `notElem` bs
prop_pre_construitZone  (ZI _ bs) b = b `notElem` bs
prop_pre_construitZone  (ZC _ bs) b = b `notElem` bs
prop_pre_construitZone  (Admin _ _) _ = True

-- PostConditions pour la fonction construitZone
prop_post_construitZone :: Zone -> Zone -> Batiment -> Bool
prop_post_construitZone (ZR f bs) (ZR f' bs') b = f == f' && b `elem` bs' && length bs' == length bs + 1
prop_post_construitZone (ZI f bs) (ZI f' bs') b = f == f' && b `elem` bs' && length bs' == length bs + 1
prop_post_construitZone (ZC f bs) (ZC f' bs') b = f == f' && b `elem` bs' && length bs' == length bs + 1
prop_post_construitZone (Admin f _) (Admin f' b') b = f == f' && b == b'
prop_post_construitZone _ _ _ = False
