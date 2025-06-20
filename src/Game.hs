{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      :  CLIGame.Character
-- Maintainer  :  Salvador Fernández <salferdez@gmail.com>
module Game
  ( valInRange,
    initializeMonsterHP,
    playerCommand,
    attack,
    run,
    encounter,
    initializePlayer,
    gauntlet,
    loop,
    formatRecord,
    parseRecords,
    writeRecord,
    readAndSortRecords,
    formatSortedRecord,
  )
where

import Character
import Control.Lens
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GameState
import HUD
import Monster
import System.Directory (renameFile)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- | Used for calculating random damage or hp
valInRange :: Int -> Int -> IO Int
valInRange start end = randomRIO (start, end)

-- | Takes current gamestate (monsterHP set to 0 or undefined), returns updated gamestate with calculated monsterHP
initializeMonsterHP :: GameState -> Monster -> IO GameState
initializeMonsterHP s m = do
  hp <- valInRange (m ^. minMonsterDamage) (m ^. maxMonsterDamage)
  return $ set monsterHP hp s

-- | Parsing and applying command ( ATTACK OR RUN )
playerCommand :: GameState -> Monster -> IO GameState
playerCommand s m = do
  if s ^. (character . hp) <= 0
    then do
      deathMessage s m
      return s
    else do
      actionMessage s
      line <- getLine
      case words line of
        ["ATTACK"] -> attack s m
        ["RUN"] -> run s m
        _ -> do
          putStrLn "Command not accepted. Try again"
          playerCommand s m

-- | \'Runs\' attack command
attack :: GameState -> Monster -> IO GameState
attack s m = do
  playerDamage <- valInRange (s ^. character . minDamage) (s ^. character . maxDamage)
  playerAttackMessage playerDamage
  if (s ^. monsterHP) - playerDamage <= 0
    then do
      let newState = set monsterHP 0 s
      return newState
    else do
      monsterDamage <- valInRange (m ^. minMonsterDamage) (m ^. maxMonsterDamage)
      monsterAttackMessage m monsterDamage
      let newPlayer = set hp (s ^. character . hp - monsterDamage) (s ^. character)
      let newState = set character newPlayer (set monsterHP (s ^. monsterHP - playerDamage) s)
      playerCommand newState m

-- | \'Runs\' run command
run :: GameState -> Monster -> IO GameState
run s m = do
  playerRunMessage m
  return s

-- | Used for setting combat
encounter :: GameState -> Monster -> IO GameState
encounter s m = do
  encounterMessage m
  newerState <- initializeMonsterHP s m
  playerCommand newerState m

-- | Initializes player (calls Character.playerCharacter) and returns updated Gamestate
initializePlayer :: IO GameState
initializePlayer = do
  character <- playerCharacter
  let newState = GameState {_character = character, _monsterHP = 100}
  return newState

-- | Game loop until player dies or runs. Keeps track of # of monsters killed
gauntlet :: Int -> GameState -> IO (Int, GameState)
gauntlet record s = do
  monsters <- readMonsters
  monster <- extract $ chooseRandom monsters
  newState <- encounter s monster
  if newState ^. monsterHP == 0
    then do
      killMessage monster
      gauntlet (record + 1) newState
    else do
      if s ^. (character . hp) <= 0
        then do
          putStrLn "You died"
          return (record, newState)
        else do
          return (record, newState)

-- | Runs game loop, prints record to file, sort records according to # of kills, returns final gamestate
loop :: IO GameState
loop = do
  state <- initializePlayer
  pair <- gauntlet 0 state
  let kills = fst pair
  -- Expected to be run from directory 'CLIGame'
  writeRecord "./data/records.txt" state kills
  readAndSortRecords "./data/records.txt"
  return $ snd pair

-- | Format printed to text file
formatRecord :: GameState -> Int -> String
formatRecord state kills = "RECORD || Character: " ++ charName ++ " Class: " ++ charClass ++ " Kills: " ++ show kills
  where
    charName = state ^. (character . name)
    charClass = show (state ^. (character . characterClass))

-- | Writes formated record to file
writeRecord :: FilePath -> GameState -> Int -> IO ()
writeRecord file state kills = appendFile file (formatRecord state kills ++ "\n")

-- Shouldn´t fail, handling error with Maybe just in case (instance of Show)
extractName :: [String] -> Maybe String
extractName words =
  case dropWhile (/= "Character:") words of
    (_ : name : _) -> Just name
    _ -> Nothing

extractClass :: [String] -> Maybe CharacterClass
extractClass words =
  case dropWhile (/= "Class:") words of
    (_ : cls : _) -> readMaybe cls
    _ -> Nothing

extractKills :: [String] -> Maybe Int
extractKills words =
  case dropWhile (/= "Kills:") words of
    (_ : kills : _) -> readMaybe kills
    _ -> Nothing

-- | parseRecord extracts name, class, and kills from the input string (to be used in a line from file)
parseRecords :: String -> Maybe (String, CharacterClass, Int)
parseRecords line = do
  let parts = words line
  namePart <- extractName parts
  classPart <- extractClass parts
  killsPart <- extractKills parts
  return (namePart, classPart, killsPart)

-- | Convert parsed records (String, CharacterClass, Int) into GameState and apply formatRecord
formatSortedRecord :: (String, CharacterClass, Int) -> String
formatSortedRecord (name, characterClass, kills) =
  let dummyCharacter =
        Character
          { _name = name,
            _hp = 100,
            _minDamage = 10,
            _maxDamage = 20,
            _characterClass = characterClass
          }
      dummyGameState = GameState dummyCharacter 50
   in formatRecord dummyGameState kills

-- | Read and sort records based on kills
readAndSortRecords :: FilePath -> IO ()
readAndSortRecords file = do
  -- Read the file contents
  content <- readFile file

  -- Parse the records (filter out failed parses) as [(String, CharacterClass, Int)]
  let records = mapMaybe parseRecords (lines content)

  -- Sort the records by kills in descending order
  let sortedRecords = sortBy (comparing (\(_, _, kills) -> negate kills)) records

  -- Convert each (String, CharacterClass, Int) record into a formatted string
  let formattedRecords = map formatSortedRecord sortedRecords

  -- Write to a temporary file
  let tempFile = file ++ ".tmp"
  writeFile tempFile (unlines formattedRecords)

  -- Rename the temporary file to the original file, efectively modifying the original file (don't know if it's efficient)
  renameFile tempFile file
