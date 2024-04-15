module Batiment where
import Forme
import Citoyen
import Data.Map (Map)
import qualified Data.Map as Map

-- newtype BatId = BatId Int deriving (Eq, Ord)

data Batiment =
    Cabane Forme Coord Int [CitId]
    | Atelier Forme Coord Int [CitId]
    | Epicerie Forme Coord Int [CitId]
    | Commissariat Forme Coord

