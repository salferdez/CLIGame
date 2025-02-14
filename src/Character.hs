{-# LANGUAGE TemplateHaskell #-}

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

data CharacterClass = FIGHTER | ROGUE | SORCERER | MUMMY deriving (Show, Eq)

-- Needed for readMaybe in Game module
instance Read CharacterClass where
  readsPrec _ str =
    case str of
      "FIGHTER" -> [(FIGHTER, "")]
      "ROGUE" -> [(ROGUE, "")]
      "SORCERER" -> [(SORCERER, "")]
      "MUMMY" -> [(MUMMY, "")]
      _ -> []

data Character = Character
  { _name :: String,
    _hp :: Int,
    _minDamage :: Int,
    _maxDamage :: Int,
    _CharacterClass :: CharacterClass
  }
  deriving (Show, Eq)

makeLenses ''Character

initializeCharacter :: String -> CharacterClass -> Character
initializeCharacter name characterClass
  | characterClass == FIGHTER = Character {_name = name, _hp = 100, _minDamage = 3, _maxDamage = 8, _CharacterClass = characterClass}
  | characterClass == SORCERER = Character {_name = name, _hp = 100, _minDamage = 2, _maxDamage = 12, _CharacterClass = characterClass}
  | characterClass == ROGUE = Character {_name = name, _hp = 100, _minDamage = 3, _maxDamage = 6, _CharacterClass = characterClass}
  | characterClass == MUMMY = Character {_name = name, _hp = 1, _minDamage = 0, _maxDamage = 0, _CharacterClass = characterClass}

playerCharacter :: IO Character
playerCharacter = do
  putStrLn "Insert your character's name: "
  name <- getLine
  setCharacter name

setCharacter :: String -> IO Character
setCharacter name = do
  putStrLn "Insert your character's class: FIGHTER | SORCERER | ROGUE"
  classInput <- getLine
  case classInput of
    "FIGHTER" -> return $ initializeCharacter name FIGHTER
    "SORCERER" -> return $ initializeCharacter name SORCERER
    "ROGUE" -> return $ initializeCharacter name ROGUE
    "MUMMY" -> return $ initializeCharacter name MUMMY
    _ -> do
      putStrLn "Command not accepted. Try again."
      playerCharacter
