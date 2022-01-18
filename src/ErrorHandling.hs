module ErrorHandling where

import GHC.IO.Exception (ExitCode(ExitFailure))
import System.Exit (exitWith)

usage :: String
usage = "USAGE: ./funEvalExpr \"expression\"\n\n\
\ expression: mathematical operation to compute respecting priorities.\n\
\\t You can use: + , - , * , / , ^ and ( )."

putError :: String -> IO()
putError str = putStrLn ("error: " ++ str) >> exitWith (ExitFailure 84)

putErrorWithUsage :: String -> IO()
putErrorWithUsage str = putStr ("error: " ++ str ++ "\n\n") >> putStrLn usage >> exitWith (ExitFailure 84)

putUsage :: IO()
putUsage = putStrLn usage