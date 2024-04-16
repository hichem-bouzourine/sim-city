{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Citoyen where
-- import Forme



-- import Data.Map (Map)
-- import qualified Data.Map as Map


newtype CitId = CitId String deriving (Eq, Ord) 

data Citoyen = Citoyen {
    ciNom :: CitId
}