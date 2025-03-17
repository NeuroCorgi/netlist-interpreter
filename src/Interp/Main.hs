{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interp.Main
  ( exec
  , readDesign
  , G.compile
  , readCommands
  , liftEither
  , module Interp.Control
  )
where

import System.IO
import System.IO.Temp
import System.Process
import System.Environment

import Control.Monad (foldM)

import Data.Char (isSpace)
import Data.List.Extra (trim)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (fromList, empty)
import Data.String (fromString)
import Data.Hashable (hash)

import Database.SQLite.Simple

import MyLib
import Interp.Control
import qualified Graph as G

liftEither :: MonadFail m => Either String a -> m a
liftEither = either fail return

liftMaybe :: MonadFail m => Maybe a -> m a
liftMaybe = maybe (fail "nothing") return

sanitize :: FilePath -> FilePath
sanitize ('/' : rest) = '.' : sanitize rest
sanitize (c : rest) = c : sanitize rest
sanitize [] = []

lookupCache :: FilePath -> IO (Either FilePath FilePath)
lookupCache filePath = do
  file <- withFile filePath ReadMode hGetContents'
  let h = hash $ filter (not . isSpace) file
  conn <- open "designs/cache.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS designs (hash INTEGER PRIMARY KEY, filePath TEXT)"
  r <- query conn "SELECT filePath FROM designs WHERE hash = ?" [h :: Int]
  cachePath' <- case r of
    [[ cachePath :: FilePath ]] -> return (Right cachePath)
    _ -> do
      cachePath' <- emptyTempFile "designs" (sanitize filePath ++ ".json")
      execute conn "INSERT INTO designs (hash, filePath) VALUES (?, ?)" (h :: Int, cachePath' :: String)
      return (Left cachePath')
  close conn
  return cachePath'

readDesign :: FilePath -> IO (NonEmpty Module)
readDesign filePath = do
  cachedFilePath <- lookupCache filePath
  jsonDesign <- case cachedFilePath of
    Left path -> do
      let log = path ++ ".log"
      jsonDesign <- do
        _exitCode <- withFile log WriteMode
          (\hOut -> do
              (_, _, _, ph) <- createProcess (proc "yosys" [filePath, "-p", "prep; proc; opt; write_json \"" ++ path ++ "\""])
                               { std_out = UseHandle hOut
                               , std_err = UseHandle hOut }
              return ph)
          >>= waitForProcess
        fromString <$> withFile path ReadMode hGetContents'
      hPutStr stderr "Design json file written to: "
      hPutStrLn stderr path
      return jsonDesign
    Right path -> do
      fromString <$> withFile path ReadMode hGetContents'
  modules <- liftEither $ MyLib.mods jsonDesign
  case nonEmpty modules of
    Just modules -> return modules
    Nothing -> fail "empty design"

readCommands :: FilePath -> IO [Command]
readCommands filePath =
  map (read . trim) . lines <$> withFile filePath ReadMode hGetContents'

exec :: G.Design -> [Command] -> IO G.Design
exec = foldM (\d (Command _ (SetC s) r) ->
                let nd = G.eval (G.start d s) in
                case r of
                  Just (ReadC m) | m == empty -> fail "empty read"
                  Just (ReadC m) -> if G.peek nd == m then return nd else fail ("State did not match, expected: " ++ show m ++ ", got: " ++ show (G.peek nd))
                  Nothing -> return nd)
