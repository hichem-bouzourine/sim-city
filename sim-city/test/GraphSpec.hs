module GraphSpec where

import Test.Hspec
import Graph

graphSpec :: Spec
graphSpec do 
    describe "Graph" $ do 
        describe "areAdjacent" $ do 
            it "checks if two HSegements are adjacent" $ do 
                areAdjacent (HSegement (C 1 1) 5) (HSegement (C 6 1) 5) `shouldBe` True
                    
-- let ville = Ville {
--             viZones = Map.fromList [(ZoneId 1, Route (HSegement (C 0 0) 5)), 
--                                     (ZoneId 2, Route (VSegement (C 5 0) 5)),
--                                     (ZoneId 3, Route (HSegement (C 4 (-4)) 10)),
--                                     (ZoneId 4, Route (VSegement (C 14 (4)) 15)),
--                                     (ZoneId 5, Route (HSegement (C 13 (-9)) 5))],
--             viCit = Map.empty
--           }
-- print $ areRoutesConnected ville 