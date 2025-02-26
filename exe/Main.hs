module Main where

import System.IO
import System.Environment

import Interp.Main
import qualified MyLib ( mods )
import qualified Graph as G

import Data.Map (Map, fromList, empty)
import Data.String (fromString)
import Data.List.Extra (trim)

import Control.Monad (when)

liftEither :: MonadFail m => Either String a -> m a
liftEither = either fail return

repl :: G.Design -> IO ()
repl design = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let (Command full (SetC sets) reads) = read $ trim line
  print full
  print sets
  let nd = G.eval $ G.start design sets
  let s = G.peek nd
  when full (print nd)
  case reads of
    Just (ReadC m) | m == empty -> print s
    Just (ReadC m) -> when (s /= m) $ do print s; fail "State did not match the expected one"
    Nothing -> return ()
  repl nd

main :: IO ()
main = do
  args <- getArgs
  case args of
    [designPath, controlPath] -> do
      design <- readDesign designPath
      control <- readCommands controlPath
      _ <- exec design control
      return ()
    [designPath] -> do
      design <- readDesign designPath
      repl design
    _ -> fail "Usage: <design-path> [control-path]"
