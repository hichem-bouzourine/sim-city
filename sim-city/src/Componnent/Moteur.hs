module Componnent.Moteur where

import Componnent.Forme
import Componnent.Utils
import Componnent.Citoyen
import Componnent.Batiment
import Componnent.Ville
import Componnent.Zone
import Componnent.Etat

import Componnent.Environnement hiding (envBatiments)
import Keyboard (Keyboard)


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- - Fonction tourEtat corrigée
tourEtat :: RealFrac a => Etat -> Keyboard -> a -> Etat
tourEtat (Etat n env c h l) kbd time =
    let newEnv = case updateOccCitoyens env of
            (Env h w envBatiments (Ville z c) m) ->
                let updatecitoyen :: Map CitId Citoyen
                    updatecitoyen = Map.foldrWithKey (\k cityon acc -> Map.insert k (metAJourEtaCitoyen cityon) acc) Map.empty c
                in Env h w envBatiments (Ville z updatecitoyen) m
        nextEnv = case newEnv of
            (Env h w envBatiments v m) ->
                Env h w envBatiments v (initMap v)
    in updateImpots $ Etat (n+1) (cleanEnv $ affecteBatiment $ updateAdminstration nextEnv) c h l     -- On incrémente le nombre de tours on relance l'administration
                                                                                       -- On reassigne les batiments disponibles 
                                                                                        -- On nettoie l'environnement
                                                                                        -- On preleve les impots si necessaire
                                                                                        

  