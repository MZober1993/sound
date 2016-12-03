module EuterpeaSamples.EHelper where

import Euterpea
import Util.BasicHelper

expSMidi s m= exportMidiFile s $ toMidi $ perform $ toMusic1 m

makeMelody::[Dur] -> [Pitch] -> [Music Pitch]
makeMelody = zipWith note

melodyWithRest r p d = insertMore r $ makeMelody d p

toP :: a -> [b] -> [(b,a)]
toP octave = map (\c -> (c,octave))
