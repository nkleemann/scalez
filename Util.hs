module Util where

import Text.Read (readMaybe)
import Data.Char (toLower, toUpper)
import System.Exit (exitWith, ExitCode(..))
import Note (Note (..))
import Scale (Scale, scalez)
import qualified Data.Map as M

--
--  OUTSIDE WORLD
--  HANDLING ZONE
--

-- | Transform a string into it's Note Representation.
strToNote :: String -> Maybe Note
strToNote s =
    case splitAt 1 s of
        (noteStr, "")  -> readMaybe $ map toUpper noteStr :: Maybe Note
        (noteStr, "#") -> readMaybe $ map toUpper noteStr ++ "Sharp" :: Maybe Note
        _              -> Nothing

noteToStr :: Note -> String
noteToStr n =
    case n of
        CSharp -> "C# "
        DSharp -> "D# "
        FSharp -> "F# "
        GSharp -> "G# "
        ASharp -> "A# "
        _      -> show n ++ " "

-- | Search the local scale-DB for a Scale.
strToScale :: String -> Maybe Scale
strToScale s = M.lookup (map toLower s) scalez

usage, complain, badArgs :: IO ()
usage         = putStrLn "scalez v 1.0\nUsage: scalez <rootnote> <scale> [--sing].\
                          \nList scales: scalez --list\n"
complain      = putStrLn "Error: Faulty arguments.\n"
badArgs       = usage >> complain

listScalez :: IO ()
listScalez = mapM_ putStrLn $ M.keys scalez

exit, exitOhNo :: IO a
exit      = exitWith ExitSuccess
exitOhNo  = exitWith $ ExitFailure 1









