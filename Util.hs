module Util where

import Text.Read
import Note

str_to_note :: String -> Maybe Note
str_to_note s
    = case splitAt 1 s of
        (note_str, "")  -> readMaybe   note_str :: Maybe Note
        (note_str, "#") -> readMaybe $ note_str ++ "Sharp" :: Maybe Note
        _               -> Nothing
