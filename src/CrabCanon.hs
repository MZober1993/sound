module CrabCanon where

import Haskore.Melody
import Haskore.Music as M
import Haskore.Music.GeneralMIDI
import Helper
import Haskore.Basic.Pitch as Pit
import Prelude
import Data.Maybe

pitches::[Pit.T]
pitches = firstLine ++ secondLine ++ thirdLine

firstLine =
  toP 1 [C, Ef, G, Af] ++
  [(0, B)] ++ toP 1 [G, Fs, F, E, Ef, D, Df, C]

secondLine =
  toP 0 [B, G, B] ++
  toP 1 [ F, Ef, D, C, Ef, G, F, G] ++
  [(2, C)] ++
  toP 1 [G, Ef, D, Ef, F, G, A, B] ++
  [(2, C)] ++ toP 1 [Ef, F, G, Af, D, Ef, F, G, F, Ef, D]

thirdLine =
  toP 1 [Ef, F, G, Af, Bf, Af, G, F, G, Af, Bf] ++
  [(2, C), (2, Df)] ++
  toP 1 [Bf, Af, G, A, B] ++
  toP 2 [C, D, Ef, C] ++
  toP 1 [B, A, B] ++
  toP 2 [C, D, Ef, F, D] ++
  [(1, G)] ++
  toP 2 [D, C, D, Ef, F, Ef, D, C] ++ [(1, B)] ++ [(2, C)] ++ toP 1 [G, Ef, C]

pitchTuple :: [(Pit.T, Atom (Haskore.Melody.Note ()))] -- necessary for retro,invert,retroInvert and invertRetro
pitchTuple = toTuple pitches

convert::[Pit.T] -> [Haskore.Melody.T ()]
convert l = toNote qn (Prelude.take 5 l) ++ [rest en] ++ toNote qn (takeAfter 5 5 l) ++ toNote en (takeAfter 10 7 l) ++ toNote qn (takeAfter 17 4 l)
    ++ toNote sn (takeAfter 21 64 l) ++ toNote en (takeAfter 85 4 l)
--TODO: clean up: work on 1 list only with take and drop

takeAfter::Int -> Int -> [a] -> [a]
takeAfter after n l = Prelude.take n $ snd $ splitAt after l

original =  line $ convert pitches
together1 = original =:= M.reverse original -- super-retrograde (
together2 = original =:= M.reverse (convertWithF invert) --invertRetro
together3 = original =:= M.reverse (line $ convert $ Prelude.reverse retrInvPs) --retroInvert
together2' = original =:= convertWithF invertRetro -- retro from invertRetro doesn't work
together3' = original =:= convertWithF retroInvert -- retro from retroInvert doesn't work

retrInvPs = changePsWithL invert $ Prelude.reverse pitchTuple

convertWithF f= line $ convert $ changePs f
changePs::([(Pit.T, Atom (Haskore.Melody.Note ()))] -> [(Pit.T, Atom (Haskore.Melody.Note ()))]) -> [Pit.T]
changePs f = map (notePitch_ . fromMaybe undefined) $ snd $ unzip $ f pitchTuple

changePsWithL::([(Pit.T, Atom (Haskore.Melody.Note ()))] -> [(Pit.T, Atom (Haskore.Melody.Note ()))]) -> [(Pit.T, Atom (Haskore.Melody.Note ()))] -> [Pit.T]
changePsWithL f l = map (notePitch_ . fromMaybe undefined) $ snd $ unzip $ f l

invertExample = invert $ toTuple $ toP 1 [C,Cs,D,Ds,E,Es,F,Fs,G,Gs,A,As,B,Bs]
