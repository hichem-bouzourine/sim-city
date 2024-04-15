module Forme 
    ( Coord(..)
    , Forme(..)
    , limites
    , appartient
    , adjacent
    ) where

data Coord = C {
    cx :: Int,
    cy :: Int
} deriving (Show, Eq)

data Forme = 
    HSegment Coord Int 
    | VSegment Coord Int 
    | Rectangle Coord Int Int 
    deriving (Show, Eq)

-- Calcul des limites d'une forme
limites :: Forme -> (Int, Int, Int, Int)
limites (HSegment (C x y) l) = (y, y, x, x + l - 1)
limites (VSegment (C x y) l) = (y - l + 1, y, x, x)
limites (Rectangle (C x y) w h) = (y - h + 1, y, x, x + w - 1)

-- Appartenance de coordonnées à une forme
appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegment (C xx yy) l) = y == yy && x >= xx && x <= xx + l - 1
appartient (C x y) (VSegment (C xx yy) l) = x == xx && y >= yy - l + 1 && y <= yy
appartient (C x y) (Rectangle (C xx yy) w h) = x >= xx && x <= xx + w - 1 && y >= yy - h + 1 && y <= yy

-- Adjacence de coordonnées à une forme
adjacent ::Coord -> Forme -> Bool
adjacent (C cx cy) (HSegment (C x y) l) =
    (cy == y) && ((cx >= x) || (cx <= x+l))
adjacent (C cx cy) (VSegment (C x y) l) =
    (cx == x) && ((cy >= y) || (cy == y-l))
adjacent (C cx cy) (Rectangle (C x y) w h) =
    (cx == x) && ((cy > y) && (cy < y-h))
    || (cx == x+w) && ((cy > y) && (cy < y-h)) 
    || (cy == y) && ((cx > x) && (cx < x+w)) 
    || (cy == y-h) && ((cx > x) && (cx < x+w)) 
