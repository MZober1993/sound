module AdditionalCrab where

import CrabCanon
import Haskore.Melody
import Haskore.Music as M
import Haskore.Basic.Duration
import Haskore.Music.GeneralMIDI
import Helper
import Haskore.Basic.Pitch as Pit
import Prelude as P
import Data.Maybe

together2 = original =:= M.reverse (convertWithF invert) --invertRetro
together3 = original =:= M.reverse (line $ convert $ P.reverse retrInvPs) --retroInvert
together2' = original =:= convertWithF invertRetro -- retro from invertRetro doesn't work
together3' = original =:= convertWithF retroInvert -- retro from retroInvert doesn't work

retrInvPs = changePsWithL invert $ P.reverse pitchTuple

convertWithF f= line $ convert $ changePs f
changePs::([(Pit.T, Atom (Haskore.Melody.Note ()))] -> [(Pit.T, Atom (Haskore.Melody.Note ()))]) -> [Pit.T]
changePs f = map (notePitch_ . fromMaybe undefined) $ snd $ unzip $ f pitchTuple

changePsWithL::([(Pit.T, Atom (Haskore.Melody.Note ()))] -> [(Pit.T, Atom (Haskore.Melody.Note ()))]) -> [(Pit.T, Atom (Haskore.Melody.Note ()))] -> [Pit.T]
changePsWithL f l = map (notePitch_ . fromMaybe undefined) $ snd $ unzip $ f l

invertExample = invert $ toTuple $ toP 1 [C,Cs,D,Ds,E,Es,F,Fs,G,Gs,A,As,B,Bs]

pitchTuple :: [(Pit.T, Atom (Haskore.Melody.Note ()))] -- necessary for retro,invert,retroInvert and invertRetro
pitchTuple = toTuple pitches