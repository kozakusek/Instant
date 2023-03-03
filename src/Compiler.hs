{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main where

import Control.Monad (when)
import Instant.Par (myLexer, pProgram)
import Instant.Print (Print, printTree)
#if C_JVM
import qualified JVM
#elif C_LLVM
import qualified LLVM
#endif
import System.Environment (getArgs)
import System.Exit (exitFailure)

runProgram :: String -> String -> IO ()
runProgram name ts =
    case pProgram $ myLexer ts of
        Left err -> do
          putStrLn err
          exitFailure
        Right program -> do 
#if C_JVM
          JVM.compile program name
#elif C_LLVM
          LLVM.compile program name
#else
          putStrLn "No language for compilation selected!"
          exitFailure
#endif

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> readFile filename >>= runProgram filename
    _ -> do
      putStrLn "Incorrect Arguments!"
      exitFailure
