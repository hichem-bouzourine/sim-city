{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
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

coord1 = C 2 2
coord2 = C 5 1
coord3 = C 0 4

forme1 = HSegement (C 1 1) 5
forme2 = VSegement (C 1 1) 5
forme3 = Rectangle (C 1 1) 5 5


appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegement (C xx yy) l) = (x <= x+l) && (x >= xx) && (y == yy)
appartient (C x y) (VSegement (C xx yy) l) = (x == xx) && (y <= yy) && (y >= y-l)
appartient (C x y) (Rectangle (C xx yy) l h) =(x >= xx) && (x <= x+l) && (y <= yy) && (y >= y+h)


adjacent ::Coord -> Forme -> Bool
adjacent (C x y) (HSegement (C xx yy) l) = (y == yy) && ((x >=xx) && (x <=xx+l))
adjacent (C x y) (VSegement (C xx yy) l) = (x == xx) && ((y <= yy) && (y >= yy-l))
adjacent (C x y) (Rectangle (C xx yy) l h) =
       (x == xx) && ((y <= yy) && (y >= yy-h))
    || (x == xx + l) && ((y <= yy) && (y >= yy - h))
    || (y == yy) && ((x >= xx) && (x <= xx + l))
    || (y == yy - h) && ((x >= xx) && (x <= xx + l))

-- >>> adjacent coord1 forme1
-- False
-- >>> adjacent coord2 forme1
-- True

-- >>> appartient coord1 forme1
-- False
-- >>> appartient coord2 forme1
-- True
-- >>> appartient coord3 forme1
-- False


collision_approx :: Forme -> Forme -> Bool
collision_approx f1 f2 = collision' f1 f2 || collision' f2 f1

collision' :: Forme -> Forme -> Bool
collision' f1 f2 = let (y1, y2, x1, x2) = limites f1
                       (y3, y4, x3, x4) = limites f2
                   in (x1 <= x4) && (x2 >= x3) && (y1 >= y4) && (y2 <= y3)

forme4 = HSegement (C 3 3) 5

-- >>> collision_approx forme1 forme2
-- True
-- >>> collision_approx forme1 forme3
-- True
-- >>> collision_approx forme2 forme3
-- True
-- >>> collision_approx forme4 forme1
-- False


-- une fonction adjacentes :: Forme -> Forme -> Bool qui prend en entr´ee deux formes et d´ecide si les deux formes sont adjacentes 
-- (c’est-`a-dire si elles se touchent sans se superposer).
adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = adjacentes' f1 f2 || adjacentes' f2 f1

adjacentes' :: Forme -> Forme -> Bool
adjacentes' f1 f2 = let (y1, y2, x1, x2) = limites f1
                    in adjacent (C x1 y1) f2 
                    || adjacent (C x2 y2) f2 
                    || adjacent (C x1 y2) f2 
                    || adjacent (C x2 y1) f2
formeAdj1 = Rectangle (C (-2) 6) 5 5
formeAdj2 = Rectangle (C 1 1) 5 5

-- >>> adjacentes formeAdj1 formeAdj2
-- True
-- >>> adjacentes (Rectangle (C (-5) (-4)) 5 5) (Rectangle (C 1 1) 5 5)
-- False