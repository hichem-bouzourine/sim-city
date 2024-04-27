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


tourEtat :: Etat -> Etat
tourEtat (Etat n env) =
   let newEnv =case updateOccCitoyen env of
               (Env h w envBatiments (Ville z c) m) ->
                  Env h w envBatiments (Ville z updatecitoyen) m
                     where
                        -- nous allons applique metAJourEtaCitoyen a chaque citoyen
                        updatecitoyen :: Map CitId Citoyen
                        updatecitoyen =  Map.foldMapWithKey (\k cityon -> Map.insert k (metAJourEtaCitoyen cityon ) c) c
                     in  Etat (n+1) $ cleanEnv newEnv