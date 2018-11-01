module Audio where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.Process (runCommand)
import Note
import Scale

data Sound
    = S Frequency
    | Stop
    deriving (Show, Eq)


type Frequency = Float
type Pattern   = [Sound]

-- Change to 432.0 if you wish :p
a4BaseFreq :: Frequency
a4BaseFreq     = 440.0 
tvelvetoneBase :: Float
tvelvetoneBase = 1.0594630943592953

-- | Convert a Note into an audible frequency.
-- |    
-- |    f_n = f_0 * a^n
-- |    f_n     Frequency of the note n half steps away
-- |    f_0     frequency of the reference note
-- |    a       (2) ** (1/12) (twelvetone_base)
-- |    n       Number of halfsteps away from reference note
toFreq :: Note -> Frequency
toFreq note = a4BaseFreq * (tvelvetoneBase ^^ (halfSteps note))

-- | Distance to reference note in number of half steps.
halfSteps :: Note -> Int
halfSteps note = toTone note - toTone A

-- | Raise a frequency by one half step.
halfStepFq :: Frequency -> Step -> Frequency
halfStepFq freq step =
    case step of
        Half   -> freq *  tvelvetoneBase
        Whole  -> freq * (tvelvetoneBase ^ (2 :: Int))
        AugSec -> freq * (tvelvetoneBase ^ (3 :: Int))

-- | Generate a sequence of Frequencies.
freqsFromRoot :: Frequency -> Scale -> [Frequency]
freqsFromRoot = scanl halfStepFq

-- | Play back a musical pattern.
sing :: Pattern -> IO ()
sing pattern' =
    forM_ pattern'
        (\s ->
            if s == Stop
                then return ()
                else playSound s >> threadDelay 300000)

-- | Play back a sound using sox.
playSound :: Sound -> IO ()
playSound s =
    runCommand ("play -n -c1 synth 0.3 sine " ++ asString s ++ " &> /dev/null") >>
    return ()

-- | Transform Frequency sequence into a musical Pattern which we can play back.
toPattern :: [Frequency] -> Pattern
toPattern freqs =
    case freqs of
        []     -> [Stop]
        (f:fs) -> S f : toPattern fs

-- | "" will never cause problems, ever.
asString :: Sound -> String
asString s =
    case s of
        Stop -> ""
        S f  -> show f
