module Audio where

import System.Process
import Note
import Scale

type Frequency = Float

-- Change to 432 as you wish :p
a4_base_freq    = 440
-- Change if you leave 12-tone territory
tvelvetone_base = 1.0594630943592953

half_steps :: Note -> Int
half_steps note = to_tone note - to_tone A

-- | Convert a Note in SemiTone representation into an audible frequency.
-- |    f_n = f_0 * a^n
-- |    f_n ... Frequency of the note n half steps away
-- |    f_0 ... frequency of the defined, fixed note (a_4) in our case
-- |    a   ... (2) ** (1/12) = 1.0594630943592953 (twelvetone_base)
-- |    n   ... Number of halfsteps away from the fixed note (a_4)
note_to_freq :: Note -> Frequency
note_to_freq note = a4_base_freq * (tvelvetone_base ^^ (half_steps note))

freq_half_step :: Frequency -> Step -> Frequency
freq_half_step freq step
    | step == Half   = freq * tvelvetone_base
    | step == Whole  = freq * (2 * tvelvetone_base)
    | step == AugSec = freq * (3 * tvelvetone_base)


freqs_from_root_freq :: Frequency -> [Step] -> [Frequency]
freqs_from_root_freq root steps = scanl freq_half_step root steps 


-- play_freq :: Frequency -> IO ProcessHandle
-- play_freq f = runCommand "play -n synth 0.2 sine 440 &> /dev/null" >> return ()