{-# LANGUAGE OverloadedStrings #-}

module ZoneQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Componnent.Zone
import Componnent.Forme
import Componnent.Batiment
import Componnent.Utils (CitId)

-- Générateur pour Coord
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary CitId where
  arbitrary = arbitrary

-- Générateur pour Forme
instance Arbitrary Forme where
  arbitrary = oneof [
      HSegement <$> arbitrary <*> arbitrary,
      VSegement <$> arbitrary <*> arbitrary,
      Rectangle <$> arbitrary <*> arbitrary <*> arbitrary
    ]

-- Générateur pour Batiment

instance Arbitrary Batiment where
    arbitrary = oneof [
            Cabane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            Atelier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            Epicerie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            Commissariat <$> arbitrary <*> arbitrary
        ]

-- Générateur pour Zone
instance Arbitrary Zone where
  arbitrary = oneof [
      Eau <$> arbitrary,
      Route <$> arbitrary,
      ZR <$> arbitrary <*> arbitrary,
      ZI <$> arbitrary <*> arbitrary,
      ZC <$> arbitrary <*> arbitrary,
      Admin <$> arbitrary <*> arbitrary
    ]

-- Propriétés pour les invariants des zones
prop_inv_ZoneRouteForme_valid :: Zone -> Bool
prop_inv_ZoneRouteForme_valid = prop_inv_ZoneRouteForme

prop_inv_zoneBatiment_valid :: Zone -> Bool
prop_inv_zoneBatiment_valid = prop_inv_zoneBatiment

prop_inv_zoneBatimentAdj_valid :: Zone -> Bool
prop_inv_zoneBatimentAdj_valid = prop_inv_zoneBatimentAdj

prop_inv_zoneBatimentForme_valid :: Zone -> Bool
prop_inv_zoneBatimentForme_valid = prop_inv_zoneBatimentForme

prop_inv_Zone_valid :: Zone -> Bool
prop_inv_Zone_valid = prop_inv_Zone

-- Propriétés pour les préconditions et postconditions de `construitZone`
prop_pre_construitZone_valid :: Zone -> Batiment -> Bool
prop_pre_construitZone_valid = prop_pre_construitZone

prop_post_construitZone_valid :: Zone -> Batiment -> Property
prop_post_construitZone_valid zone batiment =
  forAll (return (construitZone zone batiment)) $ \result ->
    case result of
      Just newZone -> prop_post_construitZone zone newZone batiment
      Nothing -> True

-- Propriétés pour les préconditions et postconditions de `retireBatiment`
prop_pre_retireBatiment_valid :: Zone -> Batiment -> Bool
prop_pre_retireBatiment_valid = prop_pre_retireBatiment

prop_post_retireBatiment_valid :: Zone -> Batiment -> Property
prop_post_retireBatiment_valid zone batiment =
  forAll (return (retireBatiment zone batiment)) $ \newZone ->
    prop_post_retireBatiment zone newZone batiment

-- Propriétés pour les préconditions et postconditions de `updateZoneBtiment`
prop_pre_updateZoneBtiment_valid :: Zone -> Batiment -> Bool
prop_pre_updateZoneBtiment_valid = prop_pre_updateZoneBtiment

prop_post_updateZoneBtiment_valid :: Zone -> Batiment -> Property
prop_post_updateZoneBtiment_valid zone batiment =
  forAll (return (updateZoneBtiment zone batiment)) $ \newZone ->
    prop_post_updateZoneBtiment zone newZone batiment

construitZoneRouteTest = do
  describe "construitZoneRoute" $ do
    it "construit une zone routier" $ property $
      forAll arbitrary $ \f ->
        case construitZoneRoute f of
          Just (Route f') -> f == f'
          _ -> False

construitZoneEauTest = do
    describe "construitZoneEau" $ do
        it "construit une zone d'eau" $ property $
            forAll arbitrary $ \f ->
                case construitZoneEau f of
                Just (Eau f') -> f == f'
                _ -> False

construitZoneResidentielleTest = do
    describe "construitZoneResidentielle" $ do
        it "construit une zone résidentielle" $ property $
            forAll genRectangleForme $ \f ->
                case construitZoneResidentielle f of
                    Just (ZR f' _) -> f == f'
                    _ -> False
  where
    genRectangleForme = Rectangle <$> arbitrary <*> arbitrary <*> arbitrary


construitZoneIndustrielleTest = do
    describe "construitZoneIndustrielle" $ do
        it "construit une zone industrielle" $ property $
            forAll genRectangleForme $ \f ->
                case construitZoneIndustrielle f of
                    Just (ZI f' _) -> f == f'
                    _ -> False
  where
    -- Generate only Rectangle forms
    genRectangleForme = Rectangle <$> arbitrary <*> arbitrary <*> arbitrary

construitZoneCommercialeTest = do
    describe "construitZoneCommerciale" $ do
        it "construit une zone commerciale" $ property $
            forAll genRectangleForme $ \f ->
                case construitZoneCommerciale f of
                    Just (ZC f' _) -> f == f'
                    _ -> False
  where
    -- Generate only Rectangle forms
    genRectangleForme = Rectangle <$> arbitrary <*> arbitrary <*> arbitrary

construitZoneAdminTest = do
    describe "construitZoneAdmin" $ do
        it "construit une zone administrative" $ property $
            forAll genRectangleForme $ \f ->
                forAll arbitrary $ \b ->
                    case construitZoneAdmin f b of
                        Just (Admin f' b') -> f == f' && b == b'
                        _ -> False
  where
    -- Generate only Rectangle forms
    genRectangleForme = Rectangle <$> arbitrary <*> arbitrary <*> arbitrary



-- addBatimentToZoneTest = do
--     describe "addBatimentToZone" $ do
--         it "ajoute un batiment à une zone" $ property $
--             forAll arbitrary $ \zone batiment ->
--                 prop_pre_updateZoneBtiment_valid zone batiment ==>
--                 prop_post_updateZoneBtiment_valid zone batiment


-- updateZoneBtimentTest = do
--     describe "updateZoneBtiment" $ do
--         it "met à jour un batiment dans une zone" $ property $
--             forAll arbitrary $ \zone batiment ->
--                 prop_pre_updateZoneBtiment_valid zone batiment ==>
--                 prop_post_updateZoneBtiment_valid zone batiment

zoneConformeTest = do
    describe "zoneConforme" $ do
        it "vérifie si une zone est conforme" $ property $
            forAll arbitrary $ \zone ->
                zoneConforme zone == prop_inv_Zone_valid zone

quickTests = do
    construitZoneRouteTest
    construitZoneEauTest
    construitZoneResidentielleTest
    construitZoneIndustrielleTest
    construitZoneCommercialeTest
    construitZoneAdminTest
    -- construitZoneTest
    -- addBatimentToZoneTest
    -- updateZoneBtimentTest
    -- zoneConformeTest