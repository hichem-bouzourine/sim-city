{-# LANGUAGE OverloadedStrings #-}
module Main where
import Forme
import Environnement
-- import Etat
import Data.Sequence (Seq)

import Control.Monad (unless)

affiche :: IO ()
affiche = putStrLn $ tableToString $ mapToTable 11 11 (initMap villeTest)


-- afficheEtat :: Etat -> IO ()
main = do
    -- putStrLn  "|__________|\n|######     |\n|           |\n|           |\n|           |\n|      X    |\n|     X     |\n|           |\n|           |\n|           |\n|           |\n|           |\n|__________|\n"

    affiche 

