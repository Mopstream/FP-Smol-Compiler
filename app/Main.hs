module Main (main) where

import Compiler
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: stack exec lab4-exe <prog path> <log path>"
    else do
      prog <- readFile $ head args
      let log = args !! 1
      writeFile log ""
      _ <- run prog log
      return ()
