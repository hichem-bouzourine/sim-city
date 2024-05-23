
module Mouse where

import SDL


import Data.Set (Set)

type Keyboard = Set Keycode

-- | création de la structure d'état de la souris 
handleEvent :: Event -> Maybe (Int, Int)
handleEvent  (Event _ (MouseButtonEvent mouseEvent)) =
  let (P (V2 x y)) = mouseButtonEventPos mouseEvent
  in Just (fromIntegral x, fromIntegral y)
handleEvent _ = Nothing
