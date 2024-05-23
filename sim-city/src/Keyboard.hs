
module Keyboard where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

-- | création de la structure d'état de clavier (vide)
createKeyboard :: Keyboard
createKeyboard = S.empty

handleEvent :: Event -> Keyboard -> Keyboard
handleEvent event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

-- | prise en compte des événements SDL2 pour mettre à jour l'état du clavier
handleEvents :: [Event] -> Keyboard -> Keyboard
handleEvents events kbd = foldl' (flip handleEvent) kbd events

-- | quelques noms de *keycode*
keycodeName :: Keycode -> Char
keycodeName KeycodeA = 'a'
keycodeName KeycodeB = 'b'
keycodeName KeycodeC = 'c'
keycodeName KeycodeD = 'd'
keycodeName KeycodeE = 'e'
keycodeName KeycodeF = 'f'
keycodeName KeycodeG = 'g'
keycodeName KeycodeH = 'h'
keycodeName KeycodeI = 'i'
keycodeName KeycodeJ = 'j'
keycodeName KeycodeK = 'k'
keycodeName KeycodeL = 'l'
keycodeName KeycodeM = 'm'
keycodeName KeycodeN = 'n'
keycodeName KeycodeO = 'o'
keycodeName KeycodeP = 'p'
keycodeName KeycodeQ = 'q'
keycodeName KeycodeR = 'r'
keycodeName KeycodeS = 's'
keycodeName KeycodeT = 't'
keycodeName KeycodeU = 'u'
keycodeName KeycodeV = 'v'
keycodeName KeycodeW = 'w'
keycodeName KeycodeX = 'x'
keycodeName KeycodeY = 'y'
keycodeName KeycodeZ = 'z'
keycodeName _ = '-'

-- | Vérifies sir le *keycode* spécificé est actuellement
-- | actif sur le clavier.
keypressed :: Keycode -> Keyboard -> Bool
keypressed kc kbd = S.member kc kbd

anyKeyPressed :: Keyboard -> Bool
anyKeyPressed kbd = not (S.null kbd)

pressedZonesKey :: Keyboard -> Bool
pressedZonesKey kb = S.member KeycodeR kb||
                    S.member KeycodeE kb ||
                    S.member KeycodeV kb ||
                    S.member KeycodeI kb ||
                    S.member KeycodeM kb 
                    
keypressedBatiment :: Keyboard -> Bool
keypressedBatiment kb = S.member KeycodeG kb ||
                        S.member KeycodeK kb ||
                        S.member KeycodeW kb
-- | Vérifies si le *keycode* spécifié est valide soit la touche A C 
valideKeyPressed :: Keyboard -> Bool
valideKeyPressed kbd = 
  S.member KeycodeR kbd || 
  S.member KeycodeC kbd || 
  S.member KeycodeE kbd || 
  S.member KeycodeV kbd ||
  S.member KeycodeI kbd || 
  S.member KeycodeM kbd || 
  S.member KeycodeA kbd ||
  S.member KeycodeK kbd ||
  S.member KeycodeW kbd ||
  S.member KeycodeG kbd 