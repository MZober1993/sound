module Helper where

import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Melody
import Haskore.Basic.Pitch as Pit
import Haskore.Basic.Duration
import Haskore.Interface.MIDI.Render as Render
import GHC.List as Ls

-- midi generation
renderTo f m = Render.fileFromGeneralMIDIMusic f song where
  song = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano m
-- play music
p piece =
  Render.playTimidity $
  MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano piece

-- pitches and tuple
toP :: a -> [b] -> [(a, b)]
toP octave = map (\c -> (octave, c))

toTuple::[Pit.T] -> [(Pit.T, Maybe (Haskore.Melody.Note ()))]
toTuple pList = zip pList $ toAtomNotes pList

toAtomNotes::[Pit.T] -> [Maybe (Haskore.Melody.Note ())]
toAtomNotes = map (Just . Note ())

toNote::Haskore.Basic.Duration.T -> [Pit.T] -> [Haskore.Melody.T ()]
toNote d =  map (\p -> note p d ())

