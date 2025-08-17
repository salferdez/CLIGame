{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module MonadicParsing
  ( Parser (..),
    parse,
    item,
    sat,
    space,
    token,
    char,
    string,
    word,
    digit,
    nat,
    int,
    extractName,
    extractClass,
    extractKills,
    parseRecord,
  )
where

import Character
import Control.Applicative
import Data.Char
import Text.Read (readMaybe)

newtype Parser a = Parser (String -> [(a, String)])

-- | Applies a parser
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) str = p str

-- | Parses a single char
item :: Parser Char
item =
  Parser
    ( \input -> case input of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  fmap f p =
    Parser
      ( \input -> case parse p input of
          [] -> []
          [(x, xs)] -> [(f x, xs)]
      )

instance Applicative Parser where
  -- \| Doesn't consume any char of the input string
  pure x = Parser (\input -> [(x, input)])

  -- \| If we try to process a parser more times than the length of the string, empty list is returned signaling failure
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parserOfFunctions <*> parserOfVariables =
    Parser
      ( \input -> case parse parserOfFunctions input of
          [] -> []
          -- x is a function of type a -> b
          [(f, xs)] -> parse (fmap f parserOfVariables) xs
      )

instance Monad Parser where
  p >>= f =
    Parser
      ( \input -> case parse p input of
          [] -> []
          [(x, xs)] -> parse (f x) xs
      )

instance Alternative Parser where
  empty = Parser (\input -> [])
  p <|> q =
    Parser
      ( \input -> case parse p input of
          [] -> parse q input
          [(x, xs)] -> [(x, xs)]
      )

-- | Parses a char that satisfies a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- | Parses and discards spaces
space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

-- \| Ignores spaces before and after applying a token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

char :: Char -> Parser Char
char c = sat (== c)

-- | Only succeeds if the entire input string is parsed at once
string :: String -> Parser String
string [] = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

-- | Parses a word (sequence of non-space chars)
word :: Parser String
word = some (sat (not . isSpace))

digit :: Parser Char
digit = sat isDigit

-- | Parses natural numbers comprising one or more digits
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- | Parses integer values
int :: Parser Int
int =
  (do
    _ <- char '-'
    n <- nat
    return (-n))
  <|> nat


-- | Extractors in parser style
extractName :: Parser String
extractName = do
  _ <- string "Character:"
  space
  word

extractClass :: Parser CharacterClass
extractClass = do
  _ <- string "Class:"
  space
  cls <- word
  case cls of
    "FIGHTER" -> return FIGHTER
    "ROGUE" -> return ROGUE
    "SORCERER" -> return SORCERER
    "MUMMY" -> return MUMMY

extractKills :: Parser Int
extractKills = do
  _ <- string "Kills:"
  space
  int

-- | Full record parser
parseRecord :: Parser (String, CharacterClass, Int)
parseRecord = do
  _ <- string "RECORD || "
  n <- extractName
  space
  c <- extractClass
  space
  k <- extractKills
  return (n, c, k)
