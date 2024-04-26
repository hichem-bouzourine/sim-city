module BatimentSpec where

import Test.Hspec
import Batiment
import Forme
import Utils

batimentSpec :: Spec
batimentSpec = do
    describe "prop_inv_capacite_batiment" $ do
        it "le nombre de citoyens d'un bâtiment est inférieur à la capacité du bâtiment" $ do
            prop_inv_capacite_batiment (Cabane (Rectangle (C 1 1) 5 5) (C 1 1) 5 []) `shouldBe` True

        it "le nombre de citoyens d'un bâtiment est inférieur à la capacité du bâtiment" $ do
            prop_inv_capacite_batiment (Cabane (Rectangle (C 1 1) 5 5) (C 1 1) 1 [CitId "1", CitId "2"]) `shouldBe` False

    -- describe "prop_inv_entre_batiment" $ do
    --     it "l'entrée des bâtiments n'est pas dans leur forme et est adjacente à leur forme" $ do
    --         prop_inv_entre_batiment (Cabane (Rectangle (C 1 1) 5 5) (C 1 1) 5 []) `shouldBe` True

    --     it "l'entrée des bâtiments n'est pas dans leur forme et est adjacente à leur forme" $ do
    --         prop_inv_entre_batiment (Cabane (Rectangle (C 1 1) 5 5) (C 2 (-1)) 5 []) `shouldBe` False

    describe "prop_inv_citoyens_distincts" $ do
        it "les citoyens d'un bâtiment sont distincts" $ do
            prop_inv_citoyens_distincts (Cabane (Rectangle (C 1 1) 5 5) (C 1 1) 5 [CitId "1", CitId "2"]) `shouldBe` True

        it "les citoyens d'un bâtiment sont distincts" $ do
            prop_inv_citoyens_distincts (Cabane (Rectangle (C 1 1) 5 5) (C 1 1) 5 [CitId "1", CitId "1"]) `shouldBe` False