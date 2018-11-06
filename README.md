# scalez

Stay in key, explore and listen to scales while staying in your comfy shell.

```
$ scalez d# blues
D# F# G# A A# C# D#

$ scalez a minor --sing
[Enchanting tune plays]

$ scalez --list
blues
dorian-mode
enigmatic
harmonic-minor
locrian-mode
lydian-mode
major
melodic-minor
minor
mixolydian-mode
neopolitan-major
neopolitan-minor
pentatonic-blues
pentatonic-major
pentatonic-minor
pentatonic-neutral
phrygian-mode
prometheus
romanian-minor
spanish-gypsy
super-locrian
```

## Installation

You need a working Haskell installation (GHC + base, stack/cabal).

1. scalez uses [sox](http://sox.sourceforge.net/) for audio playback. So you have to install it:
    * OSX
        * `$ brew install sox`
    * Ubuntu
        * `$ apt install sox`
    * Arch
        * `$ pacman -S sox`
2. Install the `process` haskell package:
    * `cabal update && cabal install process`

3. Clone this repo, compile and move scalez to your executables:
      ```
      $ git clone https://github.com/nkleemann/scalez
      $ cd scalez
      $ ghc -o scalez Main.hs
      $ cp scalez /usr/local/bin/scalez
      ```

## Usage

`$ scalez <rootnote> <scalename> [--sing]`

All notes are represented as lower- or uppercase letters. Black keys are represented with sharps:

```
C C# D D# E F F# G G# A A# B
```

You can find the list of all available scales by invoking: `scalez --list`. The name you pass has to match - ignoring upper or lower case inputs.

If you want to listen to a specific scale you pass the `--sing` flag at the end.

## Contributing

You can add a new scale by adding it to the `scalez` Map in *Scale.hs*. A Scale is modelled after a fixed sequence of intervals/steps.

## Notes
This is a WIP - I will extend the functionallity, clean up the architecture and install process and handle audio playback in a better way. Functionallity is verified under osx and gnu/linux.