{-# LANGUAGE OverloadedStrings #-}

module CitoyenQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Componnent.Citoyen
import Componnent.Batiment
import Componnent.Forme
import Componnent.Utils

-- Générateur pour Coord
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

-- Générateur pour BatId
instance Arbitrary BatId where
  arbitrary = arbitrary

instance Arbitrary Batiment where
  arbitrary =
    oneof
      [ pure (Cabane (Rectangle (C 0 0) 5 5) (C 0 6) 10 [])
      , pure (Atelier (Rectangle (C 2 3) 3 2) (C 2 6) 10 [])
      ]


-- Générateur pour Occupation
instance Arbitrary Occupation where
  arbitrary = oneof
    [ pure Travailler
    , pure Dormir
    , pure FaireCourses
    , Deplacer <$> arbitrary <*> arbitrary
    ]

-- Générateur pour Citoyen
instance Arbitrary Citoyen where
  arbitrary = oneof
    [ Immigrant <$> arbitrary <*> genEtat <*> arbitrary
    , Habitant <$> arbitrary <*> genEtat <*> genBatIds <*> arbitrary
    , Emigrant <$> arbitrary <*> arbitrary
    ]
    where
      genEtat = (,,) <$> choose (0, 100) <*> choose (0, 100) <*> choose (0, 100)
      genBatIds = (,,) <$> arbitrary <*> arbitrary <*> arbitrary


property_inv_ImmigrantIntegreVille :: Citoyen -> BatId -> Property
property_inv_ImmigrantIntegreVille c b =
  (prop_pre_integreVille_valid c b)
  ==> property $ prop_pre_integreVille c b

property_inv_TransformeEnEmigrant :: Citoyen -> Property
property_inv_TransformeEnEmigrant c =
  (prop_pre_transformeEnEmigrant_valid c)
  ==> property $ prop_pre_transformeEnEmigrant c


property_inv_AffecteBatimentTravail :: Citoyen -> BatId -> Property
property_inv_AffecteBatimentTravail c b =
  (prop_pre_affecteBatimentTravail_valid c b)
  ==> property $ prop_pre_affecteBatimentTravail' c b

-- Propriétés pour les invariants des citoyens
prop_inv_etatCitoyen_valid :: Citoyen -> Bool
prop_inv_etatCitoyen_valid = prop_inv_etatCitoyen

prop_post_integreVille_valid :: Citoyen -> Citoyen -> Bool
prop_post_integreVille_valid immigrant habitant = prop_post_integreVille immigrant habitant

-- Test des préconditions et postconditions des fonctions
prop_pre_integreVille_valid :: Citoyen -> BatId -> Bool
prop_pre_integreVille_valid (Immigrant {}) _ = True
prop_pre_integreVille_valid _ _ = False 

prop_pre_transformeEnEmigrant_valid :: Citoyen -> Bool
prop_pre_transformeEnEmigrant_valid (Habitant {}) = True
prop_pre_transformeEnEmigrant_valid _ = False

prop_post_transformeEnEmigrant_valid :: Citoyen -> Citoyen -> Bool
prop_post_transformeEnEmigrant_valid habitant emigrant = prop_post_transformeEnEmigrant habitant emigrant

prop_pre_affecteBatimentTravail_valid :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentTravail_valid (Habitant {}) _ = True
prop_pre_affecteBatimentTravail_valid _ _ = False

prop_post_affecteBatimentTravail_valid :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentTravail_valid habitantUpdated habitantUpdated' batId = prop_post_affecteBatimentTravail' habitantUpdated habitantUpdated' batId

prop_pre_affecteBatimentCourse_valid :: Citoyen -> BatId -> Bool
prop_pre_affecteBatimentCourse_valid = prop_pre_affecteBatimentCourse'

prop_post_affecteBatimentCourse_valid :: Citoyen -> Citoyen -> BatId -> Bool
prop_post_affecteBatimentCourse_valid habitantUpdated habitantUpdated' batId = prop_post_affecteBatimentCourse' habitantUpdated habitantUpdated' batId

quickTests :: Spec
quickTests = do
    describe "Property-based testing for Citoyen module" $ do
        it "Validates inv_etatCitoyen" $ property prop_inv_etatCitoyen_valid
        it "Checks precondition for integreVille" $ property property_inv_ImmigrantIntegreVille
        it "Checks postcondition for integreVille" $ property $ \citoyen batId -> prop_pre_integreVille citoyen batId ==> prop_post_integreVille_valid citoyen (integreVille citoyen batId)
        it "Checks precondition for transformeEnEmigrant" $ property property_inv_TransformeEnEmigrant
        it "Checks postcondition for transformeEnEmigrant" $ property $ \citoyen -> prop_pre_transformeEnEmigrant citoyen ==> prop_post_transformeEnEmigrant_valid citoyen (transformeEnEmigrant citoyen)
        it "Checks precondition for affecteBatimentTravail" $ property property_inv_AffecteBatimentTravail
