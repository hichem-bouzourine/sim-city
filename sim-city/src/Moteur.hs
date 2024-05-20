module Moteur where

import Forme
import Utils
import Citoyen
import Batiment
import Ville
import Zone
import Etat
import Environnement hiding (envBatiments)


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- - Fonction tourEtat corrigée
tourEtat :: Etat -> Etat
tourEtat (Etat n env) =
    let newEnv = case updateOccCitoyen env of
            (Env h w envBatiments (Ville z c) m) ->
                let updatecitoyen :: Map CitId Citoyen
                    updatecitoyen = Map.foldrWithKey (\k cityon acc -> Map.insert k (metAJourEtaCitoyen cityon) acc) Map.empty c
                in Env h w envBatiments (Ville z updatecitoyen) m
        nextEnv = case newEnv of
            (Env h w envBatiments v m) ->
                Env h w envBatiments v (initMap v)
    in Etat (n+1) $ cleanEnv $ affecteBatiment $ updateAdminstration nextEnv -- on assigne les batiments aux citoyens et nettoie l'environnement
