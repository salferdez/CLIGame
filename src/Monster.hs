{-# LANGUAGE TemplateHaskell #-}

module Monster
  ( Monster (..),
    -- Lenses
    monsterName,
    minHP,
    maxHP,
    minMonsterDamage,
    maxMonsterDamage,
    -- End lenses
    goblin,
    splitOn,
    parseMonster,
    parseMonsters,
    readMonsters,
    chooseRandom,
    extract,
  )
where

import Control.Lens
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import System.Random
import Text.Read (readMaybe)

data Monster = Monster
  { _monsterName :: String,
    _minHP :: Int,
    _maxHP :: Int,
    _minMonsterDamage :: Int,
    _maxMonsterDamage :: Int
  }

makeLenses ''Monster

-- Mummy instance
goblin :: Monster
goblin =
  Monster
    { _monsterName = "Goblin",
      _minHP = 5,
      _maxHP = 8,
      _minMonsterDamage = 3,
      _maxMonsterDamage = 9
    }

-- Used for reading monster from file, splitting each file and storing them in a list
splitOn :: Char -> String -> [String]
splitOn delim s = foldr go [""] s
  where
    go _ [] = [""]
    go c (x : xs) = if c == delim then "" : x : xs else (c : x) : xs

-- Parsing line in file, getting a monster
parseMonster :: String -> Maybe Monster
parseMonster line =
  case splitOn ':' line of
    [name, minHp, maxHp, minMonsterDamage, maxMonsterDamage] ->
      Monster name
        <$> readMaybe minHp
        <*> readMaybe maxHp
        <*> readMaybe minMonsterDamage
        <*> readMaybe maxMonsterDamage
    _ -> Nothing

-- Parsing file content, getting list of all monsters
parseMonsters :: String -> [Monster]
parseMonsters input =
  let lines' = lines input
      validLines = filter (\line -> not (null line) && not ("#" `isPrefixOf` line)) lines'
   in mapMaybe parseMonster validLines

-- Read file, parse monsters, return list of monsters
readMonsters :: IO [Monster]
readMonsters = do
  let pathFile = "./src/monsters.txt"
  content <- readFile pathFile
  let monsters = parseMonsters content
  return monsters

-- Used for choosing random monster from list
chooseRandom :: [a] -> IO (Maybe a)
chooseRandom [] = return Nothing
chooseRandom xs = do
  index <- randomRIO (0, length xs - 1)
  return (Just (xs !! index))

extract :: IO (Maybe a) -> IO a
extract ioMaybe = do
  maybeVal <- ioMaybe
  case maybeVal of
    Just x -> return x
    -- This SHOULD never happen
    Nothing -> error "extract encountered Nothing"
