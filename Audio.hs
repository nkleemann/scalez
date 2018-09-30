module Audio where

import Note
import Scale
import System.Process
import Control.Concurrent
import Control.Monad


type Frequency = Float
data Sound = S Frequency | Stop deriving (Show, Eq)
type Pattern = [Sound]

major_freqs :: [Frequency]
major_freqs = [440.0, 493.88336, 554.36536, 587.32965, 659.25525, 739.9891, 830.60974, 880.0004]

-- Change to 432 as you wish :p
a4_base_freq    = 440
tvelvetone_base = 1.0594630943592953

-- | Distance to reference note in num. of half steps
half_steps :: Note -> Int
half_steps note = to_tone note - to_tone A

-- | Convert a Note into an audible frequency.
-- |    f_n = f_0 * a^n
-- |    f_n ... Frequency of the note n half steps away
-- |    f_0 ... frequency of the reference note
-- |    a   ... (2) ** (1/12) (twelvetone_base)
-- |    n   ... Number of halfsteps away from reference note
to_freq :: Note -> Frequency
to_freq note = a4_base_freq * (tvelvetone_base ^^ (half_steps note))

-- | Raise a frequency by one half step.
freq_half_step :: Frequency -> Step -> Frequency
freq_half_step freq step
    = case step of
            Half   -> freq * tvelvetone_base
            Whole  -> freq * (tvelvetone_base ^ 2)
            AugSec -> freq * (tvelvetone_base ^ 3)

-- | Generate a sequence of Frequencies.
freqs_from_root :: Frequency -> [Step] -> [Frequency]
freqs_from_root root steps = scanl freq_half_step root steps

to_pattern :: [Frequency] -> Pattern
to_pattern freqs =
    case freqs of
        []   ->  [Stop]
        (f:fs)   ->  S f : to_pattern fs


as_string :: Sound -> String
as_string s = case s of
    Stop -> ""
    S f  -> show f

play_sound :: Sound -> IO ()
play_sound s = runCommand ("play -n -c1 synth 0.3 sine " ++ as_string s ++ " &> /dev/null") >> return ()

sing :: Pattern -> IO ()
sing pattern = do
    p <- forM pattern (\s -> do
        if s == Stop then return () else play_sound s >> threadDelay 300000)
    return ()


-- for main function 
-- sing $ to_pattern (freqs_from_root (to_freq C) (snd major))


