module Interp.Control
  ( Command(..)
  , SetC(..)
  , ReadC(..)
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isNumber, isSpace, digitToInt)
import Data.Map (Map, fromList, empty)
import Text.ParserCombinators.ReadP

space :: ReadP ()
space = do
  _ <- satisfy isSpace
  return ()

spaces :: ReadP ()
spaces = skipMany space

spaces1 :: ReadP ()
spaces1 = skipMany1 space

word :: ReadP String
word = many1 (satisfy (not . isSpace))

num :: ReadP Int
num = s2i <$> many1 (satisfy isNumber)
  where
    s2i :: String -> Int
    s2i = foldl (\a c -> 10 * a + digitToInt c) 0

numX :: ReadP (Maybe Int)
numX = (Just <$> num) <|> (Nothing <$ char 'x')

group :: ReadP a -> ReadP (String, a)
group r = do
  key <- word
  spaces1
  val <- r
  return (key, val)

newtype SetC = SetC (Map String Int) deriving Show

setC :: ReadP SetC
setC = SetC . fromList <$> sepBy1 (group num) spaces1

newtype ReadC = ReadC (Map String (Maybe Int)) deriving Show

readC :: ReadP ReadC
readC = ReadC . fromList <$> sepBy (group numX) spaces1

data Command = Command Bool SetC (Maybe ReadC) deriving Show

readCommand :: ReadP Command
readCommand = do
  setc <- setC
  spaces
  (full, readc) <- option (False, Nothing) $ do
    full <- (False <$ string "->") <|> (True <$ string "-!>")
    spaces
    readc <- readC
    return (full, Just readc)
  return (Command full setc readc)

instance Read Command where
  readsPrec _ = readP_to_S readCommand
