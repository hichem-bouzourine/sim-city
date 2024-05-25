{-# LANGUAGE OverloadedStrings #-}

module BatimentQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Componnent.Batiment
import Componnent.Forme
import Componnent.Utils (CitId)

-- Générateur pour Coord
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

-- Générateur pour CitId
instance Arbitrary CitId where
    arbitrary = arbitrary

-- Générateur pour Forme
instance Arbitrary Forme where
  arbitrary = oneof [
      HSegement <$> arbitrary <*> arbitrary,
      VSegement <$> arbitrary <*> arbitrary,
      Rectangle <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Batiment where
  arbitrary =
    oneof
      [ pure (Cabane (Rectangle (C 0 0) 5 5) (C 0 6) 10 [])
      , pure (Atelier (Rectangle (C 2 3) 3 2) (C 2 6) 10 [])
      ]



-- Propriétés pour les invariants des bâtiments
prop_inv_entre_batiment_valid :: Batiment -> Bool
prop_inv_entre_batiment_valid = prop_inv_entre_batiment

prop_inv_capacite_batiment_valid :: Batiment -> Bool
prop_inv_capacite_batiment_valid = prop_inv_capacite_batiment

prop_inv_citoyens_distincts_valid :: Batiment -> Bool
prop_inv_citoyens_distincts_valid = prop_inv_citoyens_distincts

prop_inv_Batiment_valid :: Batiment -> Bool
prop_inv_Batiment_valid = prop_inv_Batiment

-- Construire un Batiment avec des valeurs aléatoires pour les paramètres
buildRandomBatiment :: Gen Batiment
buildRandomBatiment = do
    forme <- arbitrary
    coord <- arbitrary
    cap <- arbitrary
    let maxCitizens = 10  -- Choisissez un maximum de citoyens pour limiter la génération de données
    numCitizens <- choose (0, maxCitizens)
    citIds <- vectorOf numCitizens arbitrary
    oneof [return $ Cabane forme coord cap citIds,
           return $ Atelier forme coord cap citIds,
           return $ Epicerie forme coord cap citIds,
           return $ Commissariat forme coord]

-- Propriété : Vérifier que les invariants du Batiment sont valides
prop_valid_Batiment_invariants :: Batiment -> Bool
prop_valid_Batiment_invariants b =
    prop_inv_entre_batiment_valid b

quickTests :: Spec
quickTests = do
    describe "Property-based testing for Batiment module" $ do
        -- it "Validates invariants of Batiment" $ property prop_valid_Batiment_invariants
        it "Validates inv_entre_batiment" $ property prop_inv_entre_batiment_valid
        it "Validates inv_capacite_batiment" $ property prop_inv_capacite_batiment_valid
        it "Validates inv_citoyens_distincts" $ property prop_inv_citoyens_distincts_valid
        it "Validates inv_Batiment" $ property prop_inv_Batiment_valid
        