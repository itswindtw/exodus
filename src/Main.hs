module Main where

import System.Exit
import System.Console.GetOpt
import System.Environment
import System.IO (openFile, IOMode(ReadMode), stdin)
import Data.Text.IO (hGetContents)

import PCDParser (parsePCD, extractParseError)
import PageComposer (runPCD)

data Flag = Output String deriving Show

options :: [OptDescr Flag]
options = [ Option ['o'] ["output"] (ReqArg Output "FILE") "output FILE" ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv =
  case getOpt Permute options argv of
    (as, ns, []) -> return (as, ns)
    (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
  where header = "Usage: pageComposer [-o overridedOutputPath] [inputFile]"

main :: IO ()
main = do
  (as, ns) <- getArgs >>= parseOpts
  let overridedPath = case as of
                       [Output path] -> Just path
                       _ -> Nothing
  
  inputFileHandle <- case ns of
                       [path] -> openFile path ReadMode
                       _ -> return stdin
  
  rawPCD <- hGetContents inputFileHandle

  case parsePCD rawPCD of
    Left e -> do
      putStrLn (show e)
      exitWith (ExitFailure 2)
    Right pcd -> runPCD pcd overridedPath

