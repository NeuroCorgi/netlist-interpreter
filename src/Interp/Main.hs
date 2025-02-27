module Interp.Main
  ( exec
  , readDesign
  , readCommands
  , module Interp.Control
  )
where

import System.IO
import System.IO.Temp
import System.Process
import System.Environment

import Control.Monad (foldM)
import Data.List.Extra (trim)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (fromList, empty)
import Data.String (fromString)

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

readDesign :: FilePath -> IO G.Design
readDesign filePath = do
  path <- emptyTempFile "designs" (sanitize filePath ++ ".json")
  let log = path ++ ".log"
  -- let path = "design.json"
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
  modules <- liftEither $ MyLib.mods jsonDesign
  case nonEmpty modules of
    Just modules -> liftEither $ G.compile modules
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
