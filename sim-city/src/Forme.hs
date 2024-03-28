module Forme where

data  Coord = C{
    cx :: Int ,
    cy :: Int
} deriving (Show, Eq)



data Forme = 
        HSegement Coord Int 
        | VSegement Coord Int 
        | Rectangle Coord Int Int 
    deriving (Show, Eq)

limites :: Forme -> (Int,Int,Int,Int)
limites (HSegement (C x y) l) = (x, x+l, y, y) 
limites (VSegement (C x y) l) = (x, x, y, y+l)
limites (Rectangle (C x y) w h) = (x, x+w, y, y+h)

forme1 = HSegement (C 1 2) 5


appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegement (C xx yy) l) = (x <= x+l) && (y == yy) && (x >= xx)
appartient (C x y) (VSegement (C xx yy) l) = (y <= y+l) && (x == xx) && (y >= yy)
appartient (C x y) (Rectangle (C xx yy) w h) =(x >= xx) && (y >= yy) && (x <= x+w) && (y <= y+h)

myforme = Forme (HSegement Coord ( 2 1 ) 3)

main :: IO ()
main = print (limites forme1)