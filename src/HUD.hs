{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  CLIGame.Character
-- Maintainer  :  Salvador Fernández <salferdez@gmail.com>
module HUD
  ( deathMessage,
    actionMessage,
    playerAttackMessage,
    playerRunMessage,
    monsterAttackMessage,
    encounterMessage,
    killMessage,
  )
where

import Character
import Control.Lens
import GameState
import Monster
import System.IO (hFlush, stdout)

deathMessage :: GameState -> Monster -> IO ()
deathMessage s m =
  do
    putStrLn $ "[" ++ (s ^. character . name) ++ ": 0/100]"
    putStrLn $ "The " ++ (m ^. monsterName) ++ " killed you"

actionMessage :: GameState -> IO ()
actionMessage s =
  do
    putStrLn "What do you do? ATTACK or RUN?"
    putStr ("[" ++ s ^. (character . name) ++ ": " ++ show (s ^. (character . hp)) ++ "/100]> ")
    -- Flushes to stdout just in case, sometimes it only printed one of the messages
    hFlush stdout

playerAttackMessage :: Int -> IO ()
playerAttackMessage playerDamage =
  putStrLn $ "You attack for " ++ show playerDamage ++ " damage"

playerRunMessage :: Monster -> IO ()
playerRunMessage m =
  putStrLn $ "You managed to outrun the " ++ m ^. monsterName ++ " and the horrors beyond"

monsterAttackMessage :: Monster -> Int -> IO ()
monsterAttackMessage m monsterDamage =
  putStrLn $ "The " ++ m ^. monsterName ++ " attacks you for " ++ show monsterDamage ++ " damage"

encounterMessage :: Monster -> IO ()
encounterMessage m =
  putStrLn $ "You stumble upon a " ++ m ^. monsterName

killMessage :: Monster -> IO ()
killMessage m =
  putStrLn $ "You killed the " ++ m ^. monsterName
