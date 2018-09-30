module Scale where


data Step
  = Whole
  | Half
  | AugSec
  deriving (Show, Eq)

type ScaleName = String
type Scale = (ScaleName, [Step])


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

harmonic_minor :: Scale
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

romanian_minor :: Scale
romanian_minor 
    = (
          "Romanian Minor", 
          [Whole, Half, AugSec, Half, Whole, Half, Whole]
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

-- | Named after the anciant kingdom of Phrygia in Anatolia.
phrygian_mode :: Scale
phrygian_mode 
    = (
          "Phrygian Mode", 
          [Half, Whole, Whole, Whole, Half, Whole, Whole]
      )

-- | Named after the anciant kingdom of Lydia in Anatolia.
lydian_mode :: Scale
lydian_mode 
    = (
          "Lydian Mode", 
          [Whole, Whole, Whole, Half, Whole, Whole, Half]
      )

-- | Locrian is the word used to describe the inhabitants of the ancient Greek regions of Locris.
locrian_mode :: Scale
locrian_mode 
    = (
          "Locrian Mode", 
          [Half, Whole, Whole, Half, Whole, Whole, Whole]
      )

-- | Based on the 'Mystic Chord'
prometheus :: Scale
prometheus
    = (
          "Prometheus",
          [Whole, Whole, Whole, AugSec, Half, Whole]
      )

spanish_gypsy :: Scale
spanish_gypsy
    = (
          "Spanish Gypsy", 
          [Half, AugSec, Half, Whole, Half, Whole, Whole]
      )

blues :: Scale
blues
    = (
          "Blues", 
          [AugSec, Whole, Half, Half, AugSec, Whole]
      )

super_locrian :: Scale
super_locrian
    = (
          "Super Locrian", 
          [Half, Whole, Half, Whole, Whole, Whole, Whole]
      )

-- | Still in minor, despite it's name
neopolitan_major :: Scale
neopolitan_major
    = (
          "Neopolitan Major",
          [Half, Whole, Whole, Whole, Whole, Whole, Half]  
      )

neopolitan_minor :: Scale
neopolitan_minor
    = (
          "Neopolitan Minor",
          [Half, Whole, Whole, Whole, Half, Whole, Whole]
      )


-- | originally published in a Milan journal as a musical challenge,
-- | with an invitation to harmonize it in some way.
enigmatic :: Scale
enigmatic
    = (
          "Enigmatic", 
          [Half, AugSec, Whole, Whole, Whole, Half, Half]
      )

pentatonic_neutral :: Scale
pentatonic_neutral
    = (
          "Pentatonic Neutral",
          [Whole, AugSec, Whole, AugSec]  
      )

pentatonic_major :: Scale
pentatonic_major
    = (
          "Pentatonic Major",
          [Whole, AugSec ,Whole, Whole, AugSec]
      )

pentatonic_minor :: Scale
pentatonic_minor
    = (
          "Pentatonic Minor",
          [AugSec, Whole, Whole, AugSec, Whole]  
      )

pentatonic_blues :: Scale
pentatonic_blues
    = (
          "Pentatonic Blues",
          [AugSec, Whole, Half, Half, AugSec]  
      )
