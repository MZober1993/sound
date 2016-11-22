module Helper where

import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Melody
import Haskore.Music as M
import Prelude
import Haskore.Basic.Pitch as Pit
import Haskore.Basic.Duration
import Haskore.Interface.MIDI.Render as Render
import Control.Monad
import Data.Maybe

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

changePitches::([(Pit.T, Atom (Haskore.Melody.Note ()))] -> [(Pit.T, Atom (Haskore.Melody.Note ()))]) -> [Pit.T]-> [Pit.T]
changePitches f l = map (notePitch_ . fromMaybe undefined) $ snd $ unzip $ f $ toTuple l

toAtomNotes::[Pit.T] -> [Maybe (Haskore.Melody.Note ())]
toAtomNotes = map (Just . Note ())

toNote::Haskore.Basic.Duration.T -> [Pit.T] -> [Haskore.Melody.T ()]
toNote d =  map (\p -> note p d ())

melodyWithRest r p d = insertMore r $ makeMelody p d

makeMelody::[Pit.T] -> [Haskore.Basic.Duration.T] -> [Haskore.Melody.T ()]
makeMelody = zipWith (\a b -> note a b ())

insert::Int -> a -> [a] -> [a]
insert n new_element xs = let (ys,zs) = splitAt n xs in ys ++ [new_element] ++ zs

insertMore:: [(Int,a)]->[a]->[a]
insertMore ls xs = foldl (flip $ uncurry insert) xs ls

nTimes::Int->[a]->[a]
nTimes n l = applyNTimes n (++ l) []

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]