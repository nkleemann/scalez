module Audio where

import Control.Concurrent
import Control.Monad
import System.Process
import Note
import Scale

data Sound
    = S Frequency
    | Stop
    deriving (Show, Eq)

type Frequency = Float
type Pattern   = [Sound]

-- Change to 432.0 as you wish :p
a4_base_freq    = 440.0
tvelvetone_base = 1.0594630943592953


-- | Play back a sound using sox.
play_sound :: Sound -> IO ()
play_sound s =
    runCommand ("play -n -c1 synth 0.3 sine " ++ as_string s ++ " &> /dev/null") >>
    return ()

-- | Play back a musical pattern.
sing :: Pattern -> IO ()
sing pattern = do
    p <- forM pattern
        (\s -> do
            if s == Stop
                then return ()
                else play_sound s >> threadDelay 300000)
    return ()

-- | Distance to reference note in number of half steps.
half_steps :: Note -> Int
half_steps note = to_tone note - to_tone A

-- | Convert a Note into an audible frequency.
-- |    
-- |    f_n = f_0 * a^n
-- |    f_n     Frequency of the note n half steps away
-- |    f_0     frequency of the reference note
-- |    a       (2) ** (1/12) (twelvetone_base)
-- |    n       Number of halfsteps away from reference note
to_freq :: Note -> Frequency
to_freq note = a4_base_freq * (tvelvetone_base ^^ (half_steps note))

-- | Raise a frequency by one half step.
half_stepF :: Frequency -> Step -> Frequency
half_stepF freq step =
    case step of
        Half   -> freq * tvelvetone_base
        Whole  -> freq * (tvelvetone_base ^ 2)
        AugSec -> freq * (tvelvetone_base ^ 3)

-- | Generate a sequence of Frequencies.
freqs_from_root :: Frequency -> [Step] -> [Frequency]
freqs_from_root root steps = scanl half_stepF root steps

-- | Transform Frequency sequence into a musical Pattern which we can play back.
to_pattern :: [Frequency] -> Pattern
to_pattern freqs =
    case freqs of
        []     -> [Stop]
        (f:fs) -> S f : to_pattern fs

-- | "" will never cause problems, ever.
as_string :: Sound -> String
as_string s =
    case s of
        Stop -> ""
        S f  -> show f
