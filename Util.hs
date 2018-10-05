module Util where

import Note

str_to_note :: String -> Maybe Note 
str_to_note s
    = case s of
        "C"  -> Just C
        "C#" -> Just CSharp
        "D"  -> Just D
        "D#" -> Just DSharp
        "E"  -> Just E
        "F"  -> Just F
        "F#" -> Just FSharp
        "G"  -> Just G
        "G#" -> Just GSharp
        "A"  -> Just A
        "A#" -> Just ASharp
        "B"  -> Just B
        _    -> Nothing
