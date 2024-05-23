module GraphSpec where

import Test.Hspec
import Componnent.Graph
import Componnent.Forme
import Componnent.Zone
import Data.Map (Map)
import qualified Data.Map as Map

graphSpec :: Spec
graphSpec = do
--   describe "buildGraph" $ do
--     it "creates a graph with the correct adjacency lists" $ do
--       let routes = Map.fromList [(ZoneId 1, Rectangle (C 0 0) 2 2), (ZoneId 2, Rectangle (C 3 0) 2 2)]
--           expectedGraph = Map.fromList [(ZoneId 1, Map.singleton (ZoneId 2) ([C 2 0], Rectangle (C 3 0) 2 2)),
--                                         (ZoneId 2, Map.singleton (ZoneId 1) ([C 2 0], Rectangle (C 0 0) 2 2))]

--       buildGraph routes `shouldBe` expectedGraph

  describe "isConnected" $ do
    it "returns True for a connected graph" $ do
      let graph = Map.fromList [(ZoneId 1, Map.singleton (ZoneId 2) ([C 2 0], Rectangle (C 3 0) 2 2)),
                                (ZoneId 2, Map.singleton (ZoneId 1) ([C 2 0], Rectangle (C 0 0) 2 2))]

      isConnected graph `shouldBe` True

    it "returns False for a disconnected graph" $ do
      let graph = Map.fromList [(ZoneId 1, Map.singleton (ZoneId 2) ([C 2 0], Rectangle (C 3 0) 2 2))]

      isConnected graph `shouldBe` False

  describe "manhattanDistance" $ do
    it "calculates the Manhattan distance correctly" $ do
      manhattanDistance (C 0 0) (C 3 4) `shouldBe` 7
      manhattanDistance (C 1 1) (C 1 1) `shouldBe` 0
