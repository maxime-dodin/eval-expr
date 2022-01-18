module Main where

import System.Environment.Blank (getArgs)
import Text.Printf (printf)
import EvalExpr (evalExpr)
import GHC.IO.Exception (ExitCode(ExitFailure))
import System.Exit (exitWith)
import ErrorHandling (putError, putErrorWithUsage, putUsage)

main :: IO ()
main = getArgs >>= launch
    where
        launch ["-h"] = putUsage
        launch ["--help"] = putUsage
        launch [x] = case evalExpr x of
            Left s -> putError s
            Right x -> printf "%.2f\n" x
        launch _ = putErrorWithUsage "wrong arguments"