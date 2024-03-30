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

limites :: Forme -> (Int,Int,Int,Int) -- y du Nord, y du Sud, x du gauche x du droite.
limites (HSegement (C x y) l) = (y, y, x, x+l ) 
limites (VSegement (C x y) l) = (y, y-l, x, x )
limites (Rectangle (C x y) l h) = ( y, y-h, x, x+l)

forme1 = HSegement (C 1 1) 5


appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegement (C xx yy) l) = (x <= x+l) && (x >= xx) && (y == yy) 
appartient (C x y) (VSegement (C xx yy) l) = (x == xx) && (y >= yy) && (y <= y-l)
appartient (C x y) (Rectangle (C xx yy) l h) =(x >= xx) && (x <= x+l) && (y >= yy) && (y <= y+h)


adjacent ::Coord -> Forme -> Bool
adjacent (C cx cy) (HSegement (C x y) l) =
    (cy == y) && ((cx >= x) || (cx <= x+l))
adjacent (C cx cy) (VSegement (C x y) l) =
    (cx == x) && ((cy >= y) || (cy == y-l))
adjacent (C cx cy) (Rectangle (C x y) w h) =
    (cx == x) && ((cy > y) && (cy < y-h))
    || (cx == x+w) && ((cy > y) && (cy < y-h)) 
    || (cy == y) && ((cx > x) && (cx < x+w)) 
    || (cy == y-h) && ((cx > x) && (cx < x+w)) 

