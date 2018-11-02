module Main where

import Util
import System.Environment (getArgs)
import Audio              (freqsFromRoot, sing, toFreq, toPattern)
import Note               (Note, genScalePattern)
import Scale              (Scale)
    

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
        (Just n, Just s, PrintNotes) -> mapM_ (putStr . noteToStr) (genScalePattern n s) >> putStrLn ""
        (Just n, Just s, Sing)       -> sing $ toPattern $ freqsFromRoot (toFreq n) s
        (_, _, ListScales)           -> listScalez >> exit
        (_, _, ShowUsage)            -> usage      >> exit
        (_, _, BadArgs)              -> badArgs    >> exit
        _                            -> badArgs    >> exitOhNo
