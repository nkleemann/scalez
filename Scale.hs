module Scale where

data Step
  = Whole
  | Half
  | AugSec
  deriving (Show)

type ScaleName = String
type Scale = (ScaleName, [Step])


  --
  --      LOCAL SCALEZ
  --

major :: Scale
major = ("Major", [Whole, Whole, Half, Whole, Whole, Whole, Half])

minor :: Scale
minor = ("Minor", [Whole, Half, Whole, Whole, Half, Whole, Whole])

harmonic_minor :: Scale
harmonic_minor =
  ("Harmonic Minor", [Whole, Half, Whole, Whole, Half, AugSec, Half])

melodic_minor :: Scale
melodic_minor =
  ("Melodic Minor", [Whole, Half, Whole, Whole, Whole, Whole, Half])

dorian_mode :: Scale
dorian_mode = ("Dorian Mode", [Whole, Half, Whole, Whole, Whole, Half, Whole])
