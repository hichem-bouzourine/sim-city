{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as  Map

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))
import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM
import Sprite (Sprite)
import qualified Sprite as S
import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM
import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Mouse as Ms
import qualified Debug.Trace as T

import Componnent.Etat as E
import Componnent.Forme as F
import Componnent.Zone as Z
import Componnent.Batiment as B
import Componnent.Citoyen as C
import Componnent.Ville as V
import Componnent.Environnement as Env
import Componnent.Utils as U
import Data.Text (pack)
import qualified SDL.Font as Font
import SDL.Vect (V2(..), V4(..), Point(..))
import qualified SDL.Video.Renderer as R

-- import Model (GameState)
import qualified Componnent.Moteur as M
import SDL.Event (Event(Event), eventPayload, mouseButtonEventButton, mouseButtonEventPos)
import GHC.Base (when)
import System.Posix.Internals (puts)

import qualified SDL
import qualified SDL.Font as TTF
import SDL.Vect (V2(..), V4(..), Point(..))
import qualified SDL.Video.Renderer as R
import Data.Text (Text)
import Foreign.C.Types (CInt)
import Data.Word (Word8)


spriteForm :: Forme -> (Int, Int, Int, Int)
spriteForm (F.Rectangle (C x y) w h) = (x, y, w, h)
spriteForm _ =  error "spriteForm not implemented for this type"

loadComponnent :: Renderer-> FilePath -> TextureMap -> SpriteMap -> String -> Forme -> IO (TextureMap, SpriteMap)
loadComponnent rdr path tmap smap id forme = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let (a, b, c, d) = spriteForm forme
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea (fromIntegral a) (fromIntegral (b))  (fromIntegral c) (fromIntegral d))
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

-- Menu info
menu :: IO [String]
menu = return ["Liste des actions Possible","---------------", "R pour route", "E pour eau", "I pour industrie", "M pour commerce", "V pour residentiel", "A pour administration", "G pour epicerie", "K pour cabane", "W pour atelier", "C pour citoyen"]

renderMenuItems :: SDL.Renderer -> Font.Font -> [String] -> IO ()
renderMenuItems renderer font menuItems = do
  -- Espacement vertical entre chaque élément du menu
  let spacing = 30
  -- Génération des positions pour chaque élément du menu avec l'espacement
  let positions = [V2 10 (10 + fromIntegral i * spacing) | i <- [0..(length menuItems - 1)]]
  mapM_ (\(item, pos) -> displayText renderer font (pack item) pos (V4 0 255 255 255)) (zip menuItems positions)

rendererState :: Renderer -> Font.Font -> [String] -> IO ()
rendererState renderer font txts = do
  let startX = 1000
  let startY = 10
  let lineHeight = 20
  mapM_ (\(i, txt) -> displayText renderer font (pack txt) (V2 startX (startY + i * lineHeight)) (V4 255 255 255 255)) (zip [0..] txts)

-- | affichage de texte sur le `renderer` SDL2 
displayText :: R.Renderer -> TTF.Font -> Text -> V2 CInt -> V4 Word8 -> IO ()
displayText rdr font txt pos color = do
  surface <- TTF.solid font color txt
  texture <- R.createTextureFromSurface rdr surface
  V2 w h <- SDL.surfaceDimensions surface
  let destRect = R.Rectangle (P pos) (V2 w h)
  R.copy rdr texture Nothing (Just destRect)
  SDL.freeSurface surface
  R.destroyTexture texture

main :: IO ()
main = do
  initializeAll
  Font.initialize
  font <- Font.load "assets/Nexa-Book.ttf" 17

  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1200 800 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadComponnent renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap "background" (F.Rectangle (C 0 0) 1200 800)
  -- initialisation de l'état du jeu
  let gameState = E.initEtat 400 1200 800
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState 0 0 font

displayState :: Renderer -> TextureMap -> SpriteMap -> Etat -> IO ()
displayState renderer tmap smap gameState = do
  let env = environnement gameState
  let ville = eville env
  let citoyens = viCit ville
  let batiments = envBatiments env
  -- Affichage du fond
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  
  -- Affichage des zones
  Map.traverseWithKey (\k z -> 
    let (x,y,_,_) = spriteForm $ zoneForme z 
    in S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show k)) smap) (fromIntegral x) (fromIntegral y))) (viZones ville)
  -- Affichage des batiments
  Map.traverseWithKey (\k z ->
    let (x,y,_,_) = spriteForm $ batimentForme z
    in do
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show k)) smap) (fromIntegral x) (fromIntegral y))) batiments
  -- Affichage des citoyens
  Map.traverseWithKey (\k c ->
    let (C x y) = citoyenCoord c
    in do
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (show k)) smap) (fromIntegral x) (fromIntegral y))) citoyens

  return ()



gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Etat -> Int -> Int-> Font.Font -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState lstId nb font = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  -- Recupere la dernier coordonnee de la souris
  let coord' =  foldl' (\acc e -> Ms.handleEvent e) Nothing events

  (id', gameState', tmap', smap') <- (updateAction gameState kbd' lstId tmap smap renderer)
  (id', gameState'', tmap', smap') <- (updateClick gameState' coord' id' tmap' smap' renderer)

  -- Affichage de l'état du jeu
  displayState renderer tmap' smap' gameState'
  -- Affichage du menu
  menuItems <- menu
  renderMenuItems renderer font menuItems
  -- Affichage de l'état du jeu
  rendererState renderer font $showEtat gameState'
  
  present renderer
  -- Calcul du délai pour maintenir le framerate
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime

  when (coord' /= Nothing)
    (case coord' of
      Just (x, y) -> putStrLn $ "Coordonnee de la souris: " <> (show x) <> ", " <> (show y)
      Nothing -> putStrLn "Coordonnee de la souris: Nothing")

  let gameState''' = M.tourEtat gameState'' kbd' deltaTime
  -- showEtat gameState'''
  unless (K.keypressed KeycodeEscape kbd') $ gameLoop frameRate renderer tmap' smap' kbd' gameState''' id' (nb+1) font

-- Définition de la fonction updateAction
updateAction :: Etat -> Keyboard -> Int -> TextureMap -> SpriteMap -> Renderer -> IO (Int, Etat, TextureMap, SpriteMap)
updateAction gameState kbd id tmap smap renderer = do
  if K.valideKeyPressed kbd then
    return $ (id, updateSelected gameState $ Just kbd, tmap, smap)
  else return $ (id, gameState, tmap, smap)

  -- updateClick prend en entrée l'état du jeu, une coordonnée optionnelle, un identifiant, une map de texture, une map de sprite, et un renderer
updateClick :: Etat -> Maybe (Int, Int) -> Int -> TextureMap -> SpriteMap -> Renderer -> IO (Int, Etat, TextureMap, SpriteMap)
updateClick gameState coord id tmap smap renderer = do
    case selected gameState of
        Nothing -> do
                    return (id, gameState, tmap, smap)
        Just kb -> do
                    case coord of
                        Nothing -> return (id, gameState, tmap, smap)
                        Just (x, y) -> do 
                          putStrLn $ "Click OOK: " <> (show x) <> ", " <> (show y)
                          if K.keypressed KeycodeC kb then do
                            (tmap, smap) <- loadComponnent renderer "assets/perso.bmp" tmap smap ("CitId " ++ show id) (F.Rectangle (C x y) 20 20)
                            let gameState' = updateLastMousePosition (updateSelected gameState Nothing) Nothing
                            return $ (id+1, addImmigrant id (C x y) gameState', tmap, smap)
                          else
                            case lastMousePosition gameState of
                                Nothing -> do 
                                  putStrLn "last updated"
                                  return (id, updateLastMousePosition gameState (Just (C x y)), tmap, smap)
                                Just c@(C x1 y1)  -> do
                                    if x1 == x && y1 == y then
                                        return (id, gameState, tmap, smap)
                                    else case selected gameState of
                                        Nothing -> return (id, gameState, tmap, smap)
                                        Just kb -> do 
                                          putStrLn "building component"
                                          buildComponent kb c (C x y) id gameState tmap smap renderer

-- Cette fonction reçoit une action clavier, une position de départ et de fin, ainsi qu'un état
-- Il construit suivant l'action le composant dans la zone début fin de l'environnement de l'état actuel du jeu
buildComponent :: Keyboard -> Coord -> Coord -> Int -> Etat -> TextureMap -> SpriteMap -> Renderer -> IO (Int, Etat, TextureMap, SpriteMap)
buildComponent kbd start end id etat@(Etat n env coin _ lmp) tmap smap renderer = do
    if K.pressedZonesKey kbd 
        then do
            let rout = case buildRectangle start end of 
                        Just f -> 
                              if K.keypressed KeycodeR kbd then ("assets/route.bmp", construitZoneRoute f) 
                              else if K.keypressed KeycodeE kbd then ("assets/eau.bmp", construitZoneEau f)
                              else if K.keypressed KeycodeI kbd then ("assets/industrie.bmp", construitZoneIndustrielle f)
                              else if K.keypressed KeycodeM kbd then ("assets/commerce.bmp", construitZoneCommerciale f)
                              else if K.keypressed KeycodeV kbd then ("assets/residentiel.bmp", construitZoneResidentielle f)
                              else if K.keypressed KeycodeA kbd then ("assets/administration.bmp", case findDoor f env of
                                                                                              Just d -> construitZoneAdmin f $ Commissariat f d
                                                                                              Nothing -> Nothing)
                              else ("", Nothing)
                        Nothing -> ("", Nothing)
            case rout of
                (link , Just r) -> do
                    case (envAddZone (ZoneId id) r env) of 
                      Just e -> do
                        putStrLn $ "Construction d'une zone " ++ show start
                        (tmap, smap) <- loadComponnent renderer link tmap smap ("ZoneId " ++ show id) (zoneForme r) 
                        return (id + 1, Etat n e (coin - 10) Nothing Nothing, tmap, smap)
                      _ -> do
                          putStrLn "Error zone construction"
                          let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                          return (id, etat', tmap, smap)

                _ -> do
                  putStrLn "Error zone construction"
                  let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                  return (id, etat', tmap, smap)
                    
        else if K.keypressed KeycodeA kbd
            then do
              let adm = case buildRectangle start end of 
                              Just f -> case findDoor f env of
                                          Just d -> 
                                            construitZoneAdmin f $ Commissariat f d
                                          Nothing -> Nothing  
                              Nothing -> Nothing
              case adm of
                  Just a -> do
                    putStrLn $ "Construction d'une zone administrative " ++ show start
                    (tmap', smap') <- loadComponnent renderer "assets/administration.bmp" tmap smap ("ZoneId " ++ show id) (zoneForme a)
                    (tmap'', smap'') <- loadComponnent renderer "assets/police.bmp" tmap' smap' ("BatId " ++ show id) (zoneForme a)
                    putStrLn $ "<<Batiment entre>> " ++ show (batimentEntree(head (zoneBatiments a)))
                    putStrLn $ "<<Batiment head>> " ++ show (head (zoneBatiments a))
                    case envAddZone (ZoneId id) a env of 
                      Just newEnv -> do
                          case envAddBatiment (BatId id) (head (zoneBatiments a)) (ZoneId id) newEnv of 
                            Just newEnv' -> do
                              return (id + 1, Etat n newEnv' (coin - 10) Nothing Nothing, tmap'', smap'')
                            _ -> do
                              putStrLn "Error Administration not supported aa "
                              let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                              return (id, etat', tmap, smap)
                      _ -> do
                              putStrLn "Error Administration not supported bbb"
                              let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                              return (id, etat', tmap, smap)
                  _ -> do
                      putStrLn "Error Administration not supported c"
                      let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                      return (id, etat', tmap, smap)
        else if K.keypressedBatiment kbd
            then do
                let epi = case buildRectangle start end of 
                            Just f -> case findDoor f env of
                                        Just d -> 
                                          if K.keypressed KeycodeG kbd 
                                          then ("assets/magasin.bmp", initEpicerie f d $ air f)
                                          else if K.keypressed KeycodeK kbd 
                                          then ("assets/cabane.bmp", initCabane f d $ air f)
                                          else if K.keypressed KeycodeW kbd 
                                          then ("assets/atelier.bmp", initAtelier f d $ air f)
                                          else ("", Nothing)
                                        _ -> ("", Nothing)
                            _ -> ("", Nothing)
                case epi of
                    (link, Just e) -> do
                        case getZoneWithCoord env start of
                          Nothing -> do 
                            let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                            return (id, etat', tmap, smap)
                          Just (zid, _) -> do
                            case (envAddBatiment (BatId id) e zid env) of 
                              Just env' -> do
                                putStrLn $ "Construction d'un batiment " ++ show start
                                (tmap, smap) <- loadComponnent renderer link tmap smap ("BatId " ++ show id) (batimentForme e)
                                return (id + 1, Etat n env' (coin - 10) Nothing Nothing, tmap, smap)

                              _ -> do
                                putStrLn "Error Key not supported a"
                                let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                                return (id, etat', tmap, smap)
                    _ -> do
                      putStrLn "Error Key not supported b"
                      let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                      return (id, etat', tmap, smap)
                                else do 
                                  putStrLn "Error Key not supported c"
                                  let etat' = updateLastMousePosition (updateSelected etat Nothing) Nothing
                                  return (id, etat', tmap, smap)
