module VilleSpec where

import Test.Hspec
import Componnent.Ville
import Componnent.Forme
import Componnent.Zone
import Componnent.Citoyen
import Componnent.Utils
import Componnent.Batiment
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

-- ''
villeSpec :: Spec
villeSpec = do
    describe "prop_ville_sansCollision" $ do
        it "Vérifier qu'il n y a pas de collision entre les zones de la ville" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 1 1) 5 5) []), (ZoneId 2, ZI (Rectangle (C 10 10) 5 5) [])]) Map.empty
            prop_ville_sansCollision ville `shouldBe` True

    describe "prop_ville_routesAdj" $ do
        it "chaque zone dans une ville est adjacente au minimume a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 2) 2 1) []), (ZoneId 2, ZC (Rectangle (C 2 6) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 4) 8 1))]) Map.empty
            prop_ville_routesAdj ville `shouldBe` True

        it "chaque zone dans une ville est adjacente au minimume a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, Admin (Rectangle (C 2 (-1)) 2 2) (Commissariat (Rectangle (C 2 (-1)) 2 2) (C 2 (-3)))), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            prop_ville_routesAdj ville `shouldBe` False

    describe "prop_ville_batimentsAdj" $ do
        it "Chaque batiment est adjacent a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 2) 2 1) [Cabane (Rectangle (C 1 2) 1 1) (C 2 4) 1 []]), (ZoneId 2, ZC (Rectangle (C 2 6) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 4) 8 1))]) Map.empty
            prop_ville_batimentsAdj ville `shouldBe` True

        it "Chaque batiment est adjacent a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Cabane (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 1 [], Atelier (Rectangle (C 0 (-2)) 1 1) (C 0 (-2)) 1 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            prop_ville_batimentsAdj ville `shouldBe` False

    describe "prop_inv_citoyensDansVille" $ do
        it "Tous les citoyens dans les bâtiments doivent faire partie des citoyens de la ville" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Cabane (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 1 [CitId "1"]]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Immigrant (C 2 (-4)) (1, 1, 1) Travailler)])
            prop_inv_citoyensDansVille ville `shouldBe` True

        it "Tous les citoyens dans les bâtiments doivent faire partie des citoyens de la ville" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Cabane (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 1 [CitId "1"]]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "2", Immigrant (C 2 (-4)) (1, 1, 1) Travailler)])
            prop_inv_citoyensDansVille ville `shouldBe` False
