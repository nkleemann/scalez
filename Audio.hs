module Audio where

import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Note               (Note (..), toTone)
import Scale              (Scale, Step (..))
import System.Process     (callCommand)
import System.Info        (os)


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

-- | Build and play back a musical pattern from a root note and a scale.
sing :: Note -> Scale -> IO ()
sing n s = 
    singH playF (toPattern $ freqsFromRoot (toFreq n) s)
    where playF = case os of
            "darwin" -> playSoundDarwin
            _        -> playSoundLinux -- TODO windows?

-- | Play back a musical pattern while the playback function depends
-- | the host OS.
singH :: (Sound -> IO ()) -> Pattern -> IO ()
singH playF pattern' = 
    forM_ pattern'
        (\s ->
            if s == Stop
                then return ()
                else playF s >> threadDelay 3000)


-- | Play back a sound using sox. threadDelay blocks the current thread in on darwin.
playSoundDarwin :: Sound -> IO ()
playSoundDarwin s = callCommand ("play -q -n -c1 synth 0.2 sine " ++ asString s ++ " &> /dev/null")

-- | Play back a sound using sox. threadDelay does not work as expected on linux.
playSoundLinux :: Sound -> IO ()
playSoundLinux s = callCommand ("play -q -n -t alsa -c1 synth 0.2 sine " ++ asString s ++ " &> /dev/null && sleep 0.2")


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
