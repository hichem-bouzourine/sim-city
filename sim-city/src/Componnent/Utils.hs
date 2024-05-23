module Componnent.Utils where

newtype BatId = BatId Int deriving (Eq, Ord, Show) 
newtype CitId = CitId String deriving (Eq, Ord)

instance Show CitId where
    show (CitId s) = "CitId " ++ s

