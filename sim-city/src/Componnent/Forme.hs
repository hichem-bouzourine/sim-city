{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Componnent.Forme where
import Data.Sequence (Seq)
import Data.List (nub)
import Data.Sequence (fromList)
import Data.Foldable (toList)
import Data.List (minimumBy)
import Data.Ord (comparing)
data Coord = C {
    cx :: Int,
    cy :: Int
} deriving (Show)

instance Eq Coord where
    (C x1 y1) == (C x2 y2) = x1 == x2 && y1 == y2
instance Ord Coord where
    compare (C x1 y1) (C x2 y2) = compare (x1, y1) (x2, y2)

creatCoord :: String -> String -> Coord
creatCoord x y = C (read x) (read y)

data Forme =
        HSegement Coord Int
        | VSegement Coord Int
        | Rectangle Coord Int Int
    deriving (Show, Eq)

limites :: Forme -> (Int,Int,Int,Int) -- y du Nord, y du Sud, x du gauche x du droite.
limites (HSegement (C x y) l) = (y, y, x, x+l )
limites (VSegement (C x y) l) = (y, y+l, x, x )
limites (Rectangle (C x y) l h) = (y, y+h, x, x+l)

-- buildSegment prend en entree une coordonnée haut gauche et la coordonnée bas droite et renvoie une forme
buildSegment :: Coord -> Coord -> Maybe Forme
buildSegment (C x1 y1) (C x2 y2)
    | x1 < x2 =  Just $ HSegement (C x1 y1) (x2 - x1)
    | y1 < y2 =  Just $ VSegement (C x1 y1) (y1 - y2)
    | otherwise = Nothing

-- buildRectangle prend en entree une coordonnée haut gauche et la coordonnée bas droite et renvoie une forme
buildRectangle :: Coord -> Coord -> Maybe Forme
buildRectangle (C x1 y1) (C x2 y2) = if x1 < x2 && y1 < y2 then Just $ Rectangle (C x1 y1) (x2 - x1) (y2 - y1) else Nothing

formeOrigin :: Forme -> Coord
formeOrigin (HSegement c _) = c
formeOrigin (VSegement c _) = c
formeOrigin (Rectangle c _ _) = c
-- calcule l'air d'une forme
air :: Forme -> Int
air (HSegement _ l) = l
air (VSegement _ h) = h
air (Rectangle _ l h) = l * h

appartient :: Coord -> Forme -> Bool
appartient (C x y) (HSegement (C xx yy) l) = (x <= x+l) && (x >= xx) && (y == yy)
appartient (C x y) (VSegement (C xx yy) l) = (x == xx) && (y >= yy) && (y <= yy+l)
appartient (C x y) (Rectangle (C xx yy) l h) = (x <= xx+l) && (x >= xx) && (y >= yy) && (y <= yy+h)

distance :: Coord -> Coord -> Int
distance (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

margin :: Int
margin = 50

adjacent :: Coord -> Forme  -> Bool
adjacent (C cx cy) (HSegement (C x y) l) =
  let m = margin
  in (cy >= y - m && cy <= y + m && (cx >= x - m && cx <= x + l + m))

adjacent (C cx cy) (VSegement (C x y) l) =
  let m = margin
  in (cx >= x - m && cx <= x + m && (cy >= y - m && cy <= y + l + m))

adjacent (C cx cy) (Rectangle (C x y) w h) =
  let m = margin
  in (cx >= x - m && cx <= x + w + m && (cy >= y - m && cy <= y + h + m))


collision_approx :: Forme -> Forme -> Bool
collision_approx = collision 

-- Collision entre formes
collision :: Forme -> Forme -> Bool
collision  f1 f2 = let bordure = formeBordure f1
                   in any (\c -> appartient c f2) bordure

-- une fonction adjacentes :: Forme -> Forme -> Bool qui prend en entr´ee deux formes et d´ecide si les deux formes sont adjacentes 
-- (c’est-`a-dire si elles se touchent sans se superposer).
adjacentes :: Forme -> Forme -> Bool
adjacentes f1 f2 = (formesAdjacent f1 f2 || formesAdjacent f2 f1)

-- Cette fonction renvoie toutes les coordonnées de la bordure d'une forme
formeBordure :: Forme -> Seq Coord
formeBordure forme = fromList $ nub $ case forme of
    HSegement (C x y) length ->
        [C x' y | x' <- [x..x + length]] -- Bordures des segments horizontaux
    VSegement (C x y) length ->
        [C x y' | y' <- [y..y + length]] -- Bordures des segments verticaux
    Rectangle (C x y) width height ->
        nub $ [C x' y | x' <- [x..x + width]] ++ -- Bordure supérieure
              [C x' (y + height) | x' <- [x..x + width]] ++ -- Bordure inférieure
              [C x y' | y' <- [y..y + height]] ++ -- Bordure gauche
              [C (x + width) y' | y' <- [y..y + height]] -- Bordure droite

formesAdjacent :: Forme -> Forme -> Bool
formesAdjacent f1 f2 = let bor1 = formeBordure f1
                           bor2 = formeBordure f2
                       in any (\c -> adjacent c f1) bor2 || any (\c -> adjacent c f2) bor1

-- Fonction pour vérifier si deux formes sont adjacentes et renvoyer les coordonnées de contact
formesAdjacentPoints :: Forme -> Forme -> Maybe [Coord]
formesAdjacentPoints forme1 forme2 = 
    let bord1 = formeBordure forme1
        bord2 = formeBordure forme2
        adjCoords = filter (\c -> any (`adjacent` forme2) bord1) (toList bord1)
    in if null adjCoords then Nothing else Just adjCoords


-- Cette fonction verfie si une forme est contenue dans une autre
contenue :: Forme -> Forme -> Bool
contenue f1 f2 = let bordure = formeBordure f1
                 in all (\c -> appartient c f2) bordure

-- Cette fonction verifie si une forme peut se contenir dans un size  qui a pour orrigine 0 0 en haut a gauche
contentSize :: Forme -> Int -> Int -> Bool
contentSize  f  h w = let (y1, y2, x1, x2) = limites f
                     in (x1 >= 0) && (x2 <= w) && (y2 <= h) && (y1 >= 0)

-- Cette fonction renvoie le point le plus proche entre une forme et la une autre forme d'ont il est adjacent
closestForm :: Forme -> [Forme] -> Maybe Coord
closestForm a forms =
    let formAdj = filter (adjacentes a) forms
    in if null formAdj then Nothing else let voisin =  head formAdj
                                             bordure = formeBordure voisin
                                             origine = formeOrigin a
                                             closest = minimumBy (comparing (distance origine)) bordure
                                         in Just closest
