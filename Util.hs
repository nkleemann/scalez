module Util where

import Text.Read
import Note
import Scale

-- | Transform a string into a it's Note Representation.
str_to_note :: String -> Maybe Note
str_to_note s
    = case splitAt 1 s of
        (note_str, "")  -> readMaybe   note_str :: Maybe Note
        (note_str, "#") -> readMaybe $ note_str ++ "Sharp" :: Maybe Note
        _               -> Nothing

str_to_scale :: String -> Maybe Scale
str_to_scale s
    = undefined


scale_helper :: Maybe Note -> Maybe Scale -> Maybe [Note]
scale_helper n s
    = case n of
        Nothing -> Nothing
        Just n' -> case s of
                       Nothing -> Nothing
                       Just s' -> Just $ gen_scale n' (snd s')