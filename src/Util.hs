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
import Data.List ( intercalate )
import Data.Either.Extra ( fromRight' )

import qualified Json
import qualified Intermediate
import qualified Interpreter as Compile

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x

liftEither :: MonadFail m => Either String a -> m a
liftEither = either fail return

-- | kind of sanitizes file path to be a valid temp file identifier
--   possibly not everything is covered
sanitize :: FilePath -> FilePath
--TODO: check validity of all possible paths and all possible temp file identifiers
sanitize ('/' : rest) = '.' : sanitize rest
sanitize (c : rest) = c : sanitize rest
sanitize [] = []

lookupCache :: FilePath -> [(String, String)] -> IO (Either FilePath FilePath)
lookupCache filePath parameters = do
  let params = intercalate "," $ map (\(param, value) -> param ++ ":" ++ value) parameters
  fileHash <- withFile filePath ReadMode ((hash . (params ++) . filter (not . isSpace)) <.> hGetContents')
  conn <- open "designs/cache.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS designs (hash INTEGER PRIMARY KEY, filePath TEXT)"
  r <- query conn "SELECT filePath FROM designs WHERE hash = ?" [fileHash :: Int]
  cachePath' <- case r of
    [[ cachePath :: FilePath ]] -> return (Right cachePath)
    _ -> do
      cachePath <- emptyTempFile "designs" (sanitize filePath ++ ".json")
      execute conn "INSERT INTO designs (hash, filePath) VALUES (?, ?)" (fileHash :: Int, cachePath :: String)
      return (Left cachePath)
  close conn
  return cachePath'

-- | Reads design from a verilog file
readDesign ::
  FilePath ->
  -- ^ Path to the verilog file
  Bool ->
  -- ^ Use cached result if present
  String ->
  -- ^ Name of the top level entity
  [(String, String)] ->
  -- ^ Module parameters
  IO Intermediate.Module
readDesign filePath useCache topLevelName parameters = do
  cachedFilePath <- if useCache then lookupCache filePath parameters else
                      Left <$> emptyTempFile "designs" (sanitize filePath ++ ".json")
  jsonDesign <- case cachedFilePath of
    Left path -> do
      let logPath = path ++ ".log"
      jsonDesign <- do
        -- If the process fails, nevertheless, the cache file is already remembered. It *will* cause issues
        --TODO: fix this by not writing to the database until it is known that the json is correctly created
        _exitCode <- withFile logPath WriteMode
          (\hOut -> do
              (_, _, _, ph) <- createProcess (proc "yosys" [filePath, "-p", yosysCommand path])
                               { std_out = UseHandle hOut
                               , std_err = UseHandle hOut }
              return ph)
          >>= waitForProcess
        fromString <$> withFile path ReadMode hGetContents'
      -- When used with template haskell, it would be nice to have it as a compiler note and not random output
      --TODO: think about making it a compiler note
      hPutStr stderr "Design json file written to: "
      hPutStrLn stderr path
      return jsonDesign
    Right path -> do
      fromString <$> withFile path ReadMode hGetContents'
  liftEither $ (Intermediate.select topLevelName . Intermediate.ofJson) =<< Json.parseModule jsonDesign
  where
    -- This is a user and the user only facing side,
    -- and nobody is going to exploit the absence of escaping on themselves, right?
    --TODO: proper escaping
    yosysCommand path = intercalate "; "
      [ "hierarchy -top " ++ topLevelName
      , "prep"
      , setParameters
      , "proc"
      , "flatten"
      , "memory_map"
      , "opt"
      , "write_json \"" ++ path ++ "\""
      ]

    setParameters = intercalate "; " $ map (\(param, value) -> "chparam -set " ++ param ++ " " ++ value) parameters

compile :: Intermediate.Module -> Compile.Design
--TODO: if the module was read, it compile to design without any errors
-- this is not the sace now, yet
compile = fromRight' . Compile.compile
