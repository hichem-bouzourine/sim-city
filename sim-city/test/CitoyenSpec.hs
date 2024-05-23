module CitoyenSpec where

import Test.Hspec
import Componnent.Forme
import Componnent.Utils
import Componnent.Citoyen
import GHC.IO (evaluate)

citoyenSpec :: Spec
citoyenSpec = do
    describe "prop_inv_etatCitoyen" $ do
        it "l'état d'un citoyen est positif" $ do
            prop_inv_etatCitoyen (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) `shouldBe` True

        it "l'état d'un citoyen est négatif" $ do
            prop_inv_etatCitoyen (Habitant (C 1 1) (0, (-1), 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) `shouldBe` False

    describe "prop_inv_occupationCitoyen" $ do
        it "l'occupation d'un citoyen est valide" $ do
            prop_inv_occupationCitoyen (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) `shouldBe` True

        it "l'occupation d'un citoyen est invalide" $ do
            prop_inv_occupationCitoyen (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Nothing) FaireCourses) `shouldBe` False
            
    describe "prop_inv_citoyens" $ do
        it "le citoyen est valide" $ do
            prop_inv_citoyens (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) `shouldBe` True

        it "le citoyen est invalide" $ do
            prop_inv_citoyens (Immigrant (C 1 1) (0, 0, 0) Dormir) `shouldBe` False

    describe "integreVille" $ do -- prend un Citoyen et un BatId et retourne un Citoyen
        it "transformer un immigrant en habitant" $ do
            integreVille (Immigrant (C 1 1) (0, 0, 0) Dormir) (BatId 1) `shouldBe` Habitant (C 1 1) (0, 0, 0) (BatId 1, Nothing, Nothing) Dormir
        
        it "lève une erreur si le citoyen n'est pas un immigrant" $ do
            let habitant = Habitant (C 1 1) (0, 0, 0) (BatId 1, Nothing, Nothing) Travailler in
                evaluate (integreVille habitant (BatId 1)) `shouldThrow` anyErrorCall

    describe "transformeEnEmigrant" $ do
        it "transformer un habitant en émigrant" $ do
            transformeEnEmigrant (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) `shouldBe` Emigrant (C 1 1) Travailler
        
        it "lève une erreur si le citoyen n'est pas un habitant" $ do
            evaluate (transformeEnEmigrant (Immigrant (C 1 1) (0, 0, 0) Dormir)) `shouldThrow` anyErrorCall

    describe "affecteBatimentTravail'" $ do
        it "affecter un bâtiment de travail à un citoyen" $ do
            affecteBatimentTravail' (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) (BatId 4) `shouldBe` Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 4), Just (BatId 3)) Travailler


    describe "affecteBatimentCourse'" $ do
        it "affecter un bâtiment de course à un citoyen" $ do
            affecteBatimentCourse' (Habitant (C 1 1) (0, 0, 0) (BatId 1, Just (BatId 2), Just (BatId 3)) Travailler) (BatId 4) `shouldBe` Habitant (C 1 1) (0, 0, 0) (BatId 1, Nothing, Just (BatId 4)) FaireCourses
