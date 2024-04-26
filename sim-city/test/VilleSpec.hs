module VilleSpec where

import Test.Hspec
import Ville
import Forme
import Zone
import Citoyen
import Utils
import Batiment
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
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) []), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            prop_ville_routesAdj ville `shouldBe` True

        it "chaque zone dans une ville est adjacente au minimume a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, Admin (Rectangle (C 2 (-1)) 2 2) (Commissariat (Rectangle (C 2 (-1)) 2 2) (C 2 (-3)))), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            prop_ville_routesAdj ville `shouldBe` False

    describe "prop_ville_batimentsAdj" $ do
        it "Chaque batiment est adjacent a une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Cabane (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 1 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
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

    describe "estAdjacenteARoute" $ do
        it "Vérifier si une zone est adjacente à une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) []), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            estAdjacenteARoute (ZR (Rectangle (C 3 (-3)) 2 1) []) ville `shouldBe` True
        
        it "Vérifier si une zone est adjacente à une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) []), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            estAdjacenteARoute (ZC (Rectangle (C 7 (-2)) 1 1) []) ville `shouldBe` False
        
        it "Vérifier si une zone est adjacente à une route" $ do
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) []), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) Map.empty
            let maybeZone1 = Map.lookup (ZoneId 1) (viZones ville)
            case maybeZone1 of
                Just zone1 -> estAdjacenteARoute zone1 ville `shouldBe` True
                Nothing -> expectationFailure "Zone non trouvée dans la ville"

-- -- Vérifie si une liste de zones est une séquence de zones de route connexes
--     describe "estRouteConnexe" $ do 
--         it "Vérifier si une liste de zones est une séquence de zones de route connexes" $ do
--             let ville = Ville (Map.fromList [(ZoneId 1, Route (Rectangle (C 0 (-4)) 8 1)), (ZoneId 2, Route (Rectangle (C 8 (-1)) 1 8))]) Map.empty
--             estRouteConnexe [ZR (Rectangle (C 0 (-4)) 8 1) [], ZR (Rectangle (C 8 (-4)) 1 8) []] ville `shouldBe` True

--         it "Vérifier si une liste de zones est une séquence de zones de route connexes" $ do
--             let ville = Ville (Map.fromList [(ZoneId 1, Route (Rectangle (C 0 (-4)) 8 1)), (ZoneId 2, Route (Rectangle (C 8 (-6)) 1 3))]) Map.empty
--             estRouteConnexe [ZR (Rectangle (C 0 (-4)) 1 1) [], ZR (Rectangle (C 8 (-6)) 1 1) []] ville `shouldBe` False


-- -- Vérifie si les routes sont connexes dans la ville
--     describe "routesConnexes" $ do
--         it "Vérifier si les routes sont connexes dans la ville" $ do
--             let ville = Ville (Map.fromList [(ZoneId 1, Route (Rectangle (C 0 (-4)) 8 1)), (ZoneId 2, Route (Rectangle (C 8 (-1)) 1 8))]) Map.empty
--             routesConnexes ville `shouldBe` True

        -- it "Vérifier si les routes sont connexes dans la ville" $ do
        --     let ville = Ville (Map.fromList [(ZoneId 1, Route (Rectangle (C 0 (-4)) 8 1)), (ZoneId 2, Route (Rectangle (C 8 (-6)) 1 3))]) Map.empty
        --     routesConnexes ville `shouldBe` False