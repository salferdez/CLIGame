{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  CLIGame.Character
-- Maintainer  :  Salvador Fernández <salferdez@gmail.com>
module Character
  ( Character (..),
    -- Lenses
    name,
    hp,
    minDamage,
    maxDamage,
    characterClass,
    -- End lenses
    CharacterClass (..),
    initializeCharacter,
    playerCharacter,
  )
where

import Control.Lens
import Text.Read (readMaybe)

-- | Class of character, read instance needed for readMaybe
data CharacterClass
  = FIGHTER
  | ROGUE
  | SORCERER
  | -- | Used for testing, has 1 HP
    MUMMY
  deriving (Show, Eq, Read)

-- | Represents player's character
data Character = Character
  { _name :: String,
    _hp :: Int,
    _minDamage :: Int,
    _maxDamage :: Int,
    _characterClass :: CharacterClass
  }
  deriving (Show, Eq)

makeLenses ''Character

-- | Creates new Character based on given name and CharcterClass
initializeCharacter :: String -> CharacterClass -> Character
initializeCharacter name FIGHTER = Character {_name = name, _hp = 100, _minDamage = 3, _maxDamage = 8, _characterClass = FIGHTER}
initializeCharacter name SORCERER = Character {_name = name, _hp = 100, _minDamage = 2, _maxDamage = 12, _characterClass = SORCERER}
initializeCharacter name ROGUE = Character {_name = name, _hp = 100, _minDamage = 3, _maxDamage = 6, _characterClass = ROGUE}
initializeCharacter name MUMMY = Character {_name = name, _hp = 1, _minDamage = 0, _maxDamage = 0, _characterClass = MUMMY}

-- | Asks player for character name
-- Calls setCharacter for handling correct class input
playerCharacter :: IO Character
playerCharacter = do
  putStrLn "Insert your character's name: "
  name <- getLine
  setCharacter name

-- | Asks player for CharacterClass and calls initializeCharacter if ok.
-- Else, retries until valid CharacterClass is entered
setCharacter :: String -> IO Character
setCharacter name = do
  putStrLn "Insert your character's class: FIGHTER | SORCERER | ROGUE | MUMMY"
  classInput <- getLine
  case readMaybe classInput of
    Just characterClass -> return $ initializeCharacter name characterClass
    Nothing -> do
      putStrLn "Command not accepted. Try again."
      setCharacter name
