module ZoneSpec (spec) where

import Test.Hspec
import Componnent.Zone
import Componnent.Batiment
import Componnent.Forme

spec :: Spec
spec = do
  describe "construitZone" $ do
    it "ajoute un batiment à une zone résidentielle" $
      let zoneInitiale = ZR (Rectangle (C 0 0) 10 10) []
          batiment = Cabane (Rectangle (C 1 1) 2 2) (C 1 1) 5 []
          zoneFinale = ZR (Rectangle (C 0 0) 10 10) [batiment]
      in construitZone zoneInitiale batiment `shouldBe` Just zoneFinale

    it "ajoute un batiment à une zone industrielle" $
      let zoneInitiale = ZI (Rectangle (C 0 0) 10 10) []
          batiment = Atelier (Rectangle (C 3 3) 4 4) (C 3 3) 8 []
          zoneFinale = ZI (Rectangle (C 0 0) 10 10) [batiment]
      in construitZone zoneInitiale batiment `shouldBe` Just zoneFinale

    it "ajoute un batiment à une zone commerciale" $
      let zoneInitiale = ZC (Rectangle (C 0 0) 10 10) []
          batiment = Epicerie (Rectangle (C 5 5) 3 3) (C 5 5) 10 []
          zoneFinale = ZC (Rectangle (C 0 0) 10 10) [batiment]
      in construitZone zoneInitiale batiment `shouldBe` Just zoneFinale

    it "ajoute un batiment administratif" $
      let zoneInitiale = Admin (Rectangle (C 0 0) 10 10) (Commissariat (Rectangle (C 7 7) 3 3) (C 7 7))
          batiment = Commissariat (Rectangle (C 7 7) 3 3) (C 7 7)
          zoneFinale = Admin (Rectangle (C 0 0) 10 10) (Commissariat (Rectangle (C 7 7) 3 3) (C 7 7))
      in construitZone zoneInitiale batiment `shouldBe` Just zoneFinale

  describe "prop_inv_Zone" $ do
    it "vérifie les invariants de la zone" $
      let zone = ZR (Rectangle (C 0 0) 10 10) [Cabane (Rectangle (C 0 2) 2 2) (C 0 4) 5 []]
      in prop_inv_Zone zone `shouldBe` True

    it "vérifie les invariants de la zone" $
      let zone = ZC (Rectangle (C 0 0) 10 10) [Epicerie (Rectangle (C 0 2) 2 2) (C 0 4) 5 []]
      in prop_inv_Zone zone `shouldBe` True

    it "vérifie les invariants de la zone" $
      let zone = ZI (Rectangle (C 0 0) 10 10) [Cabane (Rectangle (C 0 10) 2 2) (C 0 10) 5 []]
      in prop_inv_Zone zone `shouldBe` False
