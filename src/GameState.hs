{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  CLIGame.Character
-- Maintainer  :  Salvador Fernández <salferdez@gmail.com>
module GameState
  ( GameState (..),
    -- Start lenses
    character,
    monsterHP,
    -- End lenses
  )
where

import Character
import Control.Lens

-- | Represents current game state
data GameState = GameState
  { _character :: Character,
    _monsterHP :: Int
  }

makeLenses ''GameState
