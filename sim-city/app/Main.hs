{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Forme
import Environnement
import Etat
import Moteur
import Data.Sequence (Seq)
import Citoyen
import Batiment
import Utils
import Zone
import Ville
import Forme (Forme(Rectangle), creatCoord)
import Etat (Etat(environnement))
import Data.Bool (Bool(True, False))
import Batiment (initCabane)
import Zone (construitZoneResidentielle, construitZoneAdmin)
import Environnement (envAddBatiment)
import Utils 
import Graph

import Control.Monad (unless, Monad (return))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.List (nub)
import Data.Maybe (catMaybes)


afficheGame :: Etat -> IO ()
afficheGame etat =  putStrLn $ showEtat etat

initGame :: IO Etat
initGame = do
    putStrLn "Bienvenue dans le jeu de la vie"
    putStrLn "Veuillez entrer la hauteur de la carte"
    -- h <- getLine
    putStrLn "Veuillez entrer la largeur de la carte"
    -- w <- getLine
    -- on convrtie h et w en int
    let h = "30"
    let w = "30"
    let env = initEnv (read h) (read w)
    let etat = Etat 0 env
    afficheGame etat
    return etat


-- Cette fonction represente les manupilation du menu 
-- il fais une demande entre 3 option ajouter zone, ajouter un batiment etc
-- et retourne l'etat de jeu
menuEtat :: Etat -> IO Etat
menuEtat etat = do
    putStrLn "Veuillez choisir une action"
    putStrLn "1. Ajouter une zone"
    putStrLn "2. Ajouter un batiment"
    putStrLn "3. Ajouter un citoyen"
    putStrLn "4. Passer au tour suivant"
    putStrLn "5. Quitter"
    action <- getLine
    case action of
        "1" -> do
            putStrLn "Veillez entrer l'identifiant de la zone"
            zoneId <- getLine
            -- let zoneId = "1"
            -- data Zone = Eau Forme
                --  | Route Forme
                --  | ZR Forme [Batiment]
                --  | ZI Forme [Batiment]
                --  | ZC Forme [Batiment]
                --  | Admin Forme Batiment
            putStrLn "Veuillez entrer le type de la zone (E, R, ZR, ZI, ZC, A) qui represente Eau, Route, Zone Residentielle, Zone Industrielle, Zone Commerciale, Admin"
            typeZone <- getLine
            -- let typeZone = "ZR"
            putStrLn "Veuillez entrer la position x puis y de la zone"
            posX <- getLine
            posY <- getLine
            -- let posX = "0"
            -- let posY = "10"
            putStrLn "Veuillez entrer la forme de la zone (H, V, R)" 
            forme <- getLine
            -- let forme = "R"

            let formeBuild = case forme of
                    "H" -> do
                        putStrLn "Veuillez entrer la longueur du segment"
                        HSegement (creatCoord posX posY) . read <$> getLine
                    "V" -> do
                        putStrLn "Veuillez entrer la hauteur du segment"
                        VSegement (creatCoord posX posY) . read <$> getLine
                    "R" -> do
                        putStrLn "Veuillez entrer la largeur du rectangle"
                        largeur <- getLine
                        -- let largeur = "10"
                        putStrLn "Veuillez entrer la hauteur du rectangle"
                        Rectangle (creatCoord posX posY) (read largeur) . read <$> getLine
            let z = do
                    forme <- formeBuild
                    case typeZone of
                        "E" -> case construitZoneEau forme of
                            Just z -> return (z, Nothing, Nothing)
                            Nothing -> error "Forme de zone invalide"

                        "R" -> case construitZoneRoute forme of
                            Just z -> return (z, Nothing, Nothing)
                            Nothing -> error "Forme de zone invalide"

                        "ZR" -> case construitZoneResidentielle forme of
                            Just z -> return (ZR forme [], Nothing, Nothing)
                            Nothing -> error "Forme de zone invalide"

                        "ZI" -> case construitZoneIndustrielle forme of
                            Just z -> return (ZI forme [], Nothing, Nothing)
                            Nothing -> error "Forme de zone invalide"

                        "ZC" -> case construitZoneCommerciale forme of
                            Just z -> return $ (ZC forme [], Nothing, Nothing)
                            Nothing -> error "Forme de zone invalide"

                        "A" -> do
                            putStrLn "Veuillez entrer l'identifiant du commissariat"
                            commissariatId <- getLine
                            putStrLn "Veuillez entrer la position du commissariat"
                            putStrLn "position x "
                            posX <- getLine
                            putStrLn "position y "
                            posY <- getLine

                            putStrLn "Veuillez entrer la largeur Commissariat"
                            largeur <- getLine
                            putStrLn "Veuillez entrer la hauteur Commissariat"
                            hauteur <- getLine
                            let posBatiment = creatCoord posX posY
                            let foromBat = Rectangle posBatiment (read largeur) (read hauteur)
                            putStrLn "Veuillez entrer la position de l'entrée du commissariat"
                            putStrLn "position x "
                            posX <- getLine
                            putStrLn "position y "
                            posY <- getLine
                            let entree = creatCoord posX posY
                            let batiment = Commissariat foromBat entree
                            case construitZoneAdmin forme batiment of
                                Just z -> return (z, Just (BatId $ read commissariatId), Just batiment)
            let env = do
                    z@(zone, _, _) <- z
                    let newEnv = case z of
                            (zone, Nothing, Nothing) -> environnement etat 
                            (zone, Just batId, Just batiment) -> putBatimentWithId batId batiment (environnement etat)
                    
                    return $ envAddZone (ZoneId (read zoneId)) zone newEnv
            let etat' = Etat (tourNB etat) <$> env
            etat'

        "2" -> do
            putStrLn "Veillez entrer l'identifiant de la zone"
            zoneId <- getLine
            putStrLn "Veuillez entrer l'identifiant du batiment"
            batimentId <- getLine
            putStrLn "Veuillez entrer le type du batiment (A, E, C, CB) qui represente  Atelier, Epicerie, Commissariat et Cabane"
            typeBatiment <- getLine
            putStrLn "Veuillez entrer la position du batiment"
            putStrLn "position x "
            posX <- getLine
            putStrLn "position y "
            posY <- getLine
            let posBatiment = creatCoord posX posY
            putStrLn "Veuillez entrer la position de l'entrée du batiment"
            putStrLn "position x "
            posX <- getLine
            putStrLn "position y "
            posY <- getLine
            let entree = creatCoord posX posY
            case typeBatiment of

                "A" -> do
                    putStrLn "Veuillez entrer la largeur de l'atelier"
                    largeur <- getLine
                    putStrLn "Veuillez entrer la hauteur de l'atelier"
                    hauteur <- getLine
                    putStrLn "Veuillez entrer la capacité du batiment"
                    capacite <- getLine

                    let forme = Rectangle posBatiment (read largeur) (read hauteur)
                    let batiment = case initCabane forme entree (read capacite) of
                                    Just b -> b
                                    Nothing -> error "Erreur lors de la création de la cabane"
                    let env = envAddBatiment (BatId $ read batimentId) batiment (ZoneId $ read zoneId) (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'
                "E" -> do
                    putStrLn "Veuillez entrer la largeur de l'epicerie"
                    largeur <- getLine
                    putStrLn "Veuillez entrer la hauteur de l'epicerie"
                    hauteur <- getLine
                    putStrLn "Veuillez entrer la capacité du batiment"
                    capacite <- getLine

                    let forme = Rectangle posBatiment (read largeur) (read hauteur)
                    let batiment = case initCabane forme entree (read capacite) of
                                    Just b -> b
                                    Nothing -> error "Erreur lors de la création de la cabane"
                    let env = envAddBatiment (BatId $ read batimentId) batiment (ZoneId $ read zoneId) (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'
                "C" -> do
                    putStrLn "Veuillez entrer la largeur Commissariat"
                    largeur <- getLine
                    putStrLn "Veuillez entrer la hauteur Commissariat"
                    hauteur <- getLine

                    let forme = Rectangle posBatiment (read largeur) (read hauteur)
                    let batiment = Commissariat forme entree
                    let env = envAddBatiment (BatId $ read batimentId) batiment (ZoneId $ read zoneId) (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'

                "CB" -> do
                    putStrLn "Veuillez entrer la largeur de la cabane"
                    largeur <- getLine
                    putStrLn "Veuillez entrer la hauteur de la cabane"
                    hauteur <- getLine
                    putStrLn "Veuillez entrer la capacité du batiment"
                    capacite <- getLine
                    let forme = Rectangle posBatiment (read largeur) (read hauteur)
                    let batiment = case initCabane forme entree (read capacite) of
                                    Just b -> b
                                    Nothing -> error "Erreur lors de la création de la cabane"
                    let env = envAddBatiment (BatId $ read batimentId) batiment (ZoneId $ read zoneId) (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'
        "3" -> do
            putStrLn "Veuillez entrer l'identifiant du citoyen"
            citoyenId <- getLine
            putStrLn "Veuillez entrer le type de citoyen (E, H, I) Emmigrant, Habitant ou Immigran"
            typeCitoyen <- getLine
            putStrLn "Veuillez entrer la position "
            putStrLn "position x "
            posX <- getLine
            putStrLn "position y "
            posY <- getLine
            let position = creatCoord posX posY
            case typeCitoyen of
                "E" -> do
                    let citoyen = Emigrant position Dormir
                    let env = envAddCitoyen (CitId citoyenId) citoyen (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'
                "H" -> do
                    putStrLn "Veuillez entrer l'identifiant du batiment de repos"
                    batimentRepos <- getLine
                    let citoyen = Habitant position (50, 50, 50) (BatId (read batimentRepos), Nothing, Nothing) Dormir
                    let env = envAddCitoyen (CitId citoyenId) citoyen (environnement etat)
                    let etat' = Etat (tourNB etat) env
                    -- afficheGame etat'
                    return etat'
                "I" -> do
                    case getCommissariats (environnement etat) of
                        Just (batimentId, batiment) -> do                       -- on recupere le premier commissariat
                            let citoyen = Immigrant position (50, 50, 50) $ Deplacer $  batiment     -- le l'imigrant se deplace vers le commissariat
                            let env = envAddCitoyen (CitId citoyenId) citoyen (environnement etat)
                            let etat' = Etat (tourNB etat) env
                            -- afficheGame etat'
                            return etat'
                        Nothing -> do
                            putStrLn "Aucun commissariat n'est disponible"
                            return etat

        "4" -> return $ tourEtat etat
        "5" -> return etat
        _ -> do
            putStrLn "Action inconnue"
            return $ tourEtat etat

main :: IO ()
main = do
    etat <- initGame
    -- on appelle la fonction menuEtat pour afficher le menu
    -- puis on affiche le jeu a l'infinie sans relancer le main

    let loop etat = do
            etat' <- menuEtat etat
            afficheGame etat'
            unless (False) $ loop etat'
    loop etat
    



