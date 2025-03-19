{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Util( readDesign, compile ) where

import System.IO
import System.IO.Temp
import System.Process

import Database.SQLite.Simple

import Data.Char ( isSpace )
import Data.Hashable ( hash )
import Data.String ( fromString )
import Data.Either.Extra ( fromRight' )

import qualified Json as Json
import qualified Intermediate as Intermediate
import qualified Interpreter as Compile

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

readDesign :: FilePath -> String -> IO Intermediate.Module
readDesign filePath topLevelName = do
  cachedFilePath <- lookupCache filePath
  jsonDesign <- case cachedFilePath of
    Left path -> do
      let log = path ++ ".log"
      jsonDesign <- do
        _exitCode <- withFile log WriteMode
          (\hOut -> do
              (_, _, _, ph) <- createProcess (proc "yosys" [filePath, "-p", "prep; proc; flatten; memory_map; opt; write_json \"" ++ path ++ "\""])
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
  liftEither $ (Intermediate.select topLevelName . Intermediate.ofJson) =<< Json.parseModule jsonDesign

compile :: Intermediate.Module -> Compile.Design
compile = fromRight' . Compile.compile
