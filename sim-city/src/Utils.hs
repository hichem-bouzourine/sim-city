module Utils where

newtype BatId = BatId String deriving (Eq, Ord, Show) 
newtype CitId = CitId String deriving (Eq, Ord, Show) 

wFatigue :: Int
wFatigue = -2

wFaim :: Int
wFaim = - 1

wGain :: Int
wGain = 3

dFatigue :: Int
dFatigue = 5

cFaim :: Int
cFaim = -1