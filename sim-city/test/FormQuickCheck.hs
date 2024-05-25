{-# LANGUAGE OverloadedStrings #-}

module FormQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Componnent.Forme

-- Générateur pour Coord
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

-- Générateur pour Forme
instance Arbitrary Forme where
  arbitrary = oneof [
      HSegement <$> arbitrary <*> arbitrary,
      VSegement <$> arbitrary <*> arbitrary,
      Rectangle <$> arbitrary <*> arbitrary <*> arbitrary
    ]

-- Propriété pour vérifier que la fonction air est toujours positive
prop_air_positive :: Forme -> Bool
prop_air_positive forme = air forme >= 0


-- Propriété pour vérifier que la fonction collision est symétrique
prop_collision_symmetric :: Forme -> Forme -> Bool
prop_collision_symmetric f1 f2 = collision f1 f2 == collision f2 f1

-- Propriété pour vérifier que la fonction adjacentes est symétrique
prop_adjacentes_symmetric :: Forme -> Forme -> Bool
prop_adjacentes_symmetric f1 f2 = adjacentes f1 f2 == adjacentes f2 f1

-- Propriété pour vérifier que la fonction formesAdjacentPoints renvoie bien des points adjacents
prop_formesAdjacentPoints_correct :: Forme -> Forme -> Bool
prop_formesAdjacentPoints_correct f1 f2 =
  case formesAdjacentPoints f1 f2 of
    Just points -> all (\p -> adjacent p f1 || adjacent p f2) points
    Nothing -> not (adjacentes f1 f2)

-- Tests pour la fonction closestForm
prop_closestForm_correct :: Forme -> [Forme] -> Property
prop_closestForm_correct a forms =
  let adjForms = filter (adjacentes a) forms
  in not (null adjForms) ==> 
      case closestForm a forms of
        Just coord -> any (\f -> coord `appartient` f) adjForms
        Nothing -> False

-- limites (HSegement (C x y) l) = (y, y, x, x+l )

limitesTest = do
  describe "limites" $ do
    it "calculates limits for HSegement" $ property $ 
      forAll arbitrary $ \c l -> 
        let (y1, y2, x1, x2) = limites (HSegement c l)
        in y1 == y2 && x1 == cx(c) && x2 == cx(c) + l

    it "calculates limits for VSegement" $ property $
      forAll arbitrary $ \c l ->
        let (y1, y2, x1, x2) = limites (VSegement c l)
        in x1 == x2 && y1 == cy(c) && y2 == cy(c) + l

    it "calculates limits for Rectangle" $ property $
      forAll arbitrary $ \c w h ->
        let (y1, y2, x1, x2) = limites (Rectangle c w h)
        in y1 == cy(c) && y2 == cy(c) + h && x1 == cx(c) && x2 == cx(c) + w

appartientTest = do
  describe "appartient" $ do
    it "returns True for points in HSegement" $ property $
      forAll arbitrary $ \c l ->
        appartient c (HSegement c l) == (cx(c) <= cx(c) + l && cx(c) >= cx(c) && cy(c) == cy(c))

    it "returns True for points in VSegement" $ property $
      forAll arbitrary $ \c l ->
        appartient c (VSegement c l) == (cx(c) == cx(c) && cy(c) >= cy(c) && cy(c) <= cy(c) + l)

    it "returns True for points in Rectangle" $ property $
      forAll arbitrary $ \c w h ->
        appartient c (Rectangle c w h) == (cx(c) <= cx(c) + w && cx(c) >= cx(c) && cy(c) >= cy(c) && cy(c) <= cy(c) + h)

adjacentTest = do
  describe "adjacent" $ do
    it "returns True for adjacent points in HSegement" $ property $
      forAll arbitrary $ \c l ->
        adjacent c (HSegement c l) == (cy(c) >= cy(c) - margin && cy(c) <= cy(c) + margin && (cx(c) >= cx(c) - margin && cx(c) <= cx(c) + l + margin))

    it "returns True for adjacent points in VSegement" $ property $
      forAll arbitrary $ \c l ->
        adjacent c (VSegement c l) == (cx(c) >= cx(c) - margin && cx(c) <= cx(c) + margin && (cy(c) >= cy(c) - margin && cy(c) <= cy(c) + l + margin))

    it "returns True for adjacent points in Rectangle" $ property $
      forAll arbitrary $ \c w h ->
        adjacent c (Rectangle c w h) == (cx(c) >= cx(c) - margin && cx(c) <= cx(c) + w + margin && (cy(c) >= cy(c) - margin && cy(c) <= cy(c) + h + margin))

quickTests = do
  limitesTest
  appartientTest
  adjacentTest