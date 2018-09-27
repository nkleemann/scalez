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
major 
    = (
          "Major",
          [Whole, Whole, Half, Whole, Whole, Whole, Half]
      )

minor :: Scale
minor 
    = (
          "Minor", 
          [Whole, Half, Whole, Whole, Half, Whole, Whole]
      )

harmonic_minor 
    = (
          "Harmonic Minor", 
          [Whole, Half, Whole, Whole, Half, AugSec, Half]
      )

melodic_minor :: Scale
melodic_minor 
    = (
          "Melodic Minor", 
          [Whole, Half, Whole, Whole, Whole, Whole, Half]
      )

-- | Named after the Dorian Greeks.
dorian_mode :: Scale
dorian_mode 
    = (
          "Dorian Mode", 
          [Whole, Half, Whole, Whole, Whole, Half, Whole]
      )

-- | Ancient Greek Scale attributed to Sappho, the 7th-century-B.C. poet and musician.
mixolydian_mode :: Scale
mixolydian_mode 
    = (
          "Mixolydian Mode", 
          [Whole, Whole, Half, Whole, Whole, Half, Whole]
      )

pentatonic_neutral :: Scale
pentatonic_neutral
    = (
          "Pentatonic Neutral",
          [Whole,AugSec,Whole,AugSec]  
      )

pentatonic_major :: Scale
pentatonic_major
    = (
          "Pentatonic Major",
          [Whole,AugSec,Whole,Whole,AugSec]
      )

pentatonic_minor :: Scale
pentatonic_minor
    = (
          "Pentatonic Minor",
          [AugSec,Whole,Whole,AugSec,Whole]  
      )

pentatonic_blues :: Scale
pentatonic_blues
    = (
          "Pentatonic Blues",
          [AugSec,Whole,Half,Half,AugSec]  
      )







