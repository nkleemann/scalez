module Main where

import Util
import Audio              (sing)
import Note               (Note, presentScalePattern)
import Scale              (Scale)
import System.Environment (getArgs)

data Flag
  = Sing
  | PrintNotes
  | ListScales
  | ShowUsage
  | BadArgs
  deriving (Eq)


main :: IO ()
main = 
    getArgs >>= parse >>= handleArgs


parse :: [String] -> IO (Maybe Note, Maybe Scale, Flag)
parse args =
    case args of
        [r, s]            -> return (strToNote r, strToScale s, PrintNotes)
        [r, s, "--sing"]  -> return (strToNote r, strToScale s, Sing) 
        ["--list"]        -> return (Nothing, Nothing, ListScales)
        []                -> return (Nothing, Nothing, ShowUsage)
        _                 -> return (Nothing, Nothing, BadArgs)

handleArgs :: (Maybe Note, Maybe Scale, Flag) -> IO ()
handleArgs args = 
    case args of
        (Just n, Just s, PrintNotes) -> presentScalePattern n s
        (Just n, Just s, Sing)       -> sing n s
        (_, _, ListScales)           -> listScalez >> exit
        (_, _, ShowUsage)            -> usage      >> exit
        (_, _, BadArgs)              -> badArgs    >> exit
        _                            -> badArgs    >> exitOhNo
