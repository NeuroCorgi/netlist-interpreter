module Main where

import System.IO
import System.IO.Temp
import System.Process
import System.Environment

import qualified MyLib ( mods )
import qualified Graph as G

import Data.Map (Map, fromList, empty)
import Data.String ( fromString )
import Data.List.Split

import Control.Monad ((<=<), when, foldM_)

liftEither :: MonadFail m => Either String a -> m a
liftEither = either fail return

sanitize :: FilePath -> FilePath
sanitize ('/' : rest) = '.' : sanitize rest
sanitize (c : rest) = c : sanitize rest
sanitize [] = []

readDesign :: FilePath -> IO G.Design
readDesign filePath = do
  -- path <- emptyTempFile "yosys-results" (sanitize filePath ++ ".json")
  let path = "design.json"
  jsonDesign <- do
    yosysProc <- withFile "yosys_log" WriteMode $ \hOut -> do
      (_, _, _, ph) <- createProcess (proc "yosys" [filePath, "-p", "prep; proc; opt; write_json \"" ++ path ++ "\""])
                       { std_out = UseHandle hOut
                       , std_err = UseHandle hOut }
      return ph
    waitForProcess yosysProc
    fromString <$> withFile path ReadMode hGetContents'
  liftEither $ (G.compile . head) =<< MyLib.mods jsonDesign

parseOp :: (String -> a) -> String -> Map String a
parseOp r = fromList . map (\[a, b] -> (a, r b)) . chunksOf 2 . words

type SetC = Map String Int
parseSets :: String -> SetC
parseSets = parseOp read

type ReadC = Map String (Maybe Int)
parseReads :: String -> ReadC
parseReads = parseOp readR
  where
    readR :: String -> Maybe Int
    readR "x" = Nothing
    readR s = Just $ read s

type Command = (SetC, Maybe ReadC)
readCommand :: String -> Command
readCommand line =
  case splitOn "->" line of
    [sets] -> (parseSets sets, Nothing)
    [sets, reads] -> (parseSets sets, Just $ parseReads reads)
    _ -> (empty, Nothing)

readCommands :: FilePath -> IO [Command]
readCommands filePath =
  map readCommand . lines <$> withFile filePath ReadMode hGetContents'

exec :: G.Design -> [Command] -> IO ()
exec = foldM_ (\d (s, r) ->
                let nd = G.eval (G.start d s) in
                case r of
                  Just m | m == empty -> fail "empty read"
                  Just m -> if G.peek nd == m then return nd else fail ("State did not match, expected: " ++ show m ++ ", got: " ++ show (G.peek nd))
                  Nothing -> return nd)

repl :: G.Design -> IO ()
repl design = do
  putStr "> "

  line <- getLine
  putStrLn line
  let (sets, reads) = readCommand line
  let nd = G.eval $ G.start design sets
  let s = G.peek nd
  case reads of
    Just m | m == empty -> print s
    Just m -> when (s /= m) $ do print s; fail "State did not match the expected one"
    Nothing -> return ()
  repl nd

main :: IO ()
main = do
  args <- getArgs
  case args of
    [designPath, controlPath] -> do
      design <- readDesign designPath
      control <- readCommands controlPath
      print control
      exec design control
    [designPath] -> do
      design <- readDesign designPath
      repl design
    _ -> fail "Usage: <design-path> [control-path]"
  -- withFile "counter.json" ReadMode $ \h -> do
  --   text <- fromString <$> hGetContents h
  --   md <- liftEither ((G.compile . head) =<< MyLib.mods text)
  --   print md
  --   let s1 = G.eval $ G.start md (fromList [("rstn", 1), ("clk", 1)])
  --   let s2 = G.eval $ G.start s1 (fromList [("clk", 0)])
  --   let s3 = G.eval $ G.start s2 (fromList [("clk", 1)])
  --   let s4 = G.eval $ G.start s3 (fromList [("clk", 0), ("rstn", 0)])
  --   let s5 = G.eval $ G.start s4 (fromList [("clk", 1)])
  --   let s6 = G.eval $ G.start s5 (fromList [("clk", 0)])
  --   let s7 = G.eval $ G.start s6 (fromList [("clk", 1)])
  --   let s8 = G.eval $ G.start s7 (fromList [("clk", 0)])
  --   let s9 = G.eval $ G.start s8 (fromList [("clk", 1)])
  --   let s10 = G.eval $ G.start s9 (fromList [("clk", 0)])
  --   let s11 = G.eval $ G.start s10 (fromList [("clk", 1)])
  --   let s12 = G.eval $ G.start s11 (fromList [("clk", 0)])

  --   print s12
  --   print (G.peek s12)

    -- let s1 = G.eval $ G.start md (fromList [("rstn", 1), ("clk", 0)])
    -- print s1
    -- let s2 = G.eval $ G.start s1 (fromList [("clk", 1)])
    -- print s2
    -- let s3 = G.eval $ G.start s2 (fromList [("clk", 0), ("d", 1)])
    -- print s3
    -- let s4 = G.eval $ G.start s3 (fromList [("clk", 1), ("rstn", 0), ("d", 2)])
    -- let s5 = G.eval $ G.start s4 (fromList [("clk", 0)])
    -- let s6 = G.eval $ G.start s5 (fromList [("clk", 1)])
    -- let s7 = G.eval $ G.start s6 (fromList [("clk", 0)])
    -- print s7
    -- print (G.peek s7)
    -- let s8 = G.eval $ G.start s7 (fromList [("rst", 1)])
    -- print s8
    -- print (G.peek s8)
