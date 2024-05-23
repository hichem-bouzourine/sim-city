module FormeSpec where

import Test.Hspec
import Componnent.Forme

formeSpec :: Spec
formeSpec = do
  describe "limites" $ do
    it "calculates limits for HSegement" $ do
      limites (HSegement (C 1 1) 5) `shouldBe` (1, 1, 1, 6)

    it "calculates limits for VSegement" $ do
      limites (VSegement (C 1 1) 5) `shouldBe` (1, 6, 1, 1)

    it "calculates limits for Rectangle" $ do
      limites (Rectangle (C 1 1) 5 5) `shouldBe` (1, 6, 1, 6)

  describe "appartient" $ do
    it "checks if Coord belongs to HSegement" $ do
      appartient (C 3 1) (HSegement (C 1 1) 5) `shouldBe` True

    it "checks if Coord belongs to VSegement" $ do
      appartient (C 1 2) (VSegement (C 1 1) 5) `shouldBe` True

    it "checks if Coord belongs to Rectangle" $ do
      appartient (C 0 0) (Rectangle (C 1 1) 5 5) `shouldBe` False
      appartient (C 3 3) (Rectangle (C 1 1) 5 5) `shouldBe` True
  describe "collision_approx" $ do
    it "checks collision between two Formes" $ do
      collision_approx (Rectangle (C 2 2) 2 2) (Rectangle (C 3 3) 3 3) `shouldBe` True
      collision_approx (HSegement (C 1 1) 5) (Rectangle (C 3 3) 3 3) `shouldBe` False

  describe "adjacentes" $ do
    it "checks if two Formes are adjacent" $ do
      adjacentes (Rectangle (C 1 1) 5 5) (Rectangle (C 1 7) 3 3) `shouldBe` True
      adjacentes (Rectangle (C 1 1) 5 5) (Rectangle (C 4 5) 5 5) `shouldBe` False
      