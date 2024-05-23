module EnvironnementSpec where

import Test.Hspec
import Componnent.Environnement
import Componnent.Ville
import Componnent.Zone
import Componnent.Batiment
import Componnent.Forme
import Componnent.Utils
import Componnent.Citoyen
import qualified Data.Map as Map


environnementSpec :: Spec
environnementSpec = do
    describe "prop_inv_citoyen_batiment" $ do
        it "Vérifie que tout les batiments de la ville sont dans l'environnement envBatiments" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Immigrant (C 2 (-4)) (1, 1, 1) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment env `shouldBe` True
        
        it "Vérifie que tout les batiments de la ville ne sont pas dans l'environnement envBatiments" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Cabane (Rectangle (C 2 (-3)) 2 2) (C 2 (-3)) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Immigrant (C 2 (-4)) (1, 1, 1) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment env `shouldBe` False

    -- | Invariant: chaque bâtiment associé à un citoyen contient ce citoyen dans sa liste de citoyens.
    describe "prop_inv_citoyen_batiment_citoyen" $ do
        it "Vérifie que chaque bâtiment associé à un citoyen contient ce citoyen dans sa liste de citoyens" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment_citoyen env `shouldBe` True

        it "Vérifie que chaque bâtiment associé à un citoyen ne contient pas ce citoyen dans sa liste de citoyens" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [])] -- pas de citoyens associés au batiment
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment_citoyen env `shouldBe` False

    describe "prop_inv_citoyen_batiment_citoyen_batiment" $ do
        it "Vérifie que chaque citoyen associé à un bâtiment a une reference vers ce bâtiment dans ça liste de batId" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment_citoyen_batiment env `shouldBe` True

        it "Vérifie que chaque citoyen associé à un bâtiment n'a pas de reference vers ce bâtiment dans ça liste de batId" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"])] 
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 2, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            prop_inv_citoyen_batiment_citoyen_batiment env `shouldBe` False

    describe "isBatimentInEnv" $ do
        it "Vérifie si un bâtiment est dans l'environnement" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            let batiment = Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"]
            isBatimentInEnv env batiment `shouldBe` True

        it "Vérifie si un bâtiment n'est pas dans l'environnement" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 1) 3 3) (C 1 1) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            let batiment = Epicerie (Rectangle (C 2 2) 3 3) (C 2 2) 4 [CitId "1"]
            isBatimentInEnv env batiment `shouldBe` False

    describe "putBatimentWithId" $ do 
        it "Vérifie si un bâtiment est mis à jour dans l'environnement" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            let batId = BatId 1
            let batiment' = Epicerie (Rectangle (C 0 (-3)) 1 1) (C 1 (-4)) 4 [CitId "1", CitId "2"]
            let env' = putBatimentWithId batId batiment' env
            isBatimentInEnv env' batiment' `shouldBe` True

        it "Vérifie si un bâtiment n'est pas mis à jour dans l'environnement" $ do
            let height = 10
            let width = 10
            let envBatiments = Map.fromList [(BatId 1, Epicerie (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 4 [CitId "1"])]
            let ville = Ville (Map.fromList [(ZoneId 1, ZR (Rectangle (C 0 (-2)) 2 2) [Epicerie (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 4 []]), (ZoneId 2, ZC (Rectangle (C 2 (-5)) 3 2) []), (ZoneId 3, Route (Rectangle (C 0 (-4)) 8 1))]) (Map.fromList [(CitId "1", Habitant (C 2 (-4)) (1, 1, 1) (BatId 1, Nothing, Nothing) Travailler)])
            let eCarte = Map.fromList [(C 1 1, 'B')]
            let env = Env height width envBatiments ville eCarte
            let batId = BatId 1
            let batiment' = Epicerie (Rectangle (C 0 (-3)) 1 1) (C 0 (-4)) 4 [CitId "1", CitId "2"]
            let env' = putBatimentWithId batId batiment' env
            isBatimentInEnv env' (Epicerie (Rectangle (C 1 (-3)) 1 1) (C 2 (-4)) 4 [CitId "1"]) `shouldBe` False
