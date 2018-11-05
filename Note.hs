module Note where

    
import Scale (Scale, Step (..))


data Note
    = C 
    | CSharp 
    | D 
    | DSharp 
    | E 
    | F 
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B 
    deriving (Show, Read, Enum, Eq)


type SemiTone     = Int
type ScalePattern = [Note]


-- | Generate and print a scale pattern to the screen.
presentScalePattern :: Note -> Scale -> IO ()
presentScalePattern n s =
    mapM_ (putStr . noteToStr) (genScalePattern n s) >> putStrLn ""

-- | Generate a concrete scale beginning at root note.
genScalePattern :: Note -> Scale -> ScalePattern
genScalePattern = scanl transpose

-- | Transpose a note by a given step.
transpose :: Note -> Step -> Note
transpose note step
    = case step of
            Half   -> toNote . sameOctave . halfStep  $ toTone note
            Whole  -> toNote . sameOctave . wholeStep $ toTone note
            AugSec -> toNote . sameOctave . augSec    $ toTone note

-- | Perform a half step.
halfStep :: SemiTone -> SemiTone
halfStep = (+ 1)

-- | Perform a whole step.
wholeStep :: SemiTone -> SemiTone
wholeStep = (+ 2)

-- | Perform both half and whole step to form an 'augmented second'.
augSec :: SemiTone -> SemiTone
augSec = (+ 3)

-- | Keep a tone in the same octave.
sameOctave :: SemiTone -> SemiTone
sameOctave = flip mod 12

-- | Used for transposition
toTone :: Note -> SemiTone
toTone = fromEnum

-- | Transform SemiTone back into classical notation as 'Note'
toNote :: SemiTone -> Note
toNote = toEnum

-- | Print a Note to the screen.
noteToStr :: Note -> String
noteToStr n =
    case n of
        CSharp -> "C# "
        DSharp -> "D# "
        FSharp -> "F# "
        GSharp -> "G# "
        ASharp -> "A# "
        _      -> show n ++ " "
