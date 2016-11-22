module CrabCanon where

import Haskore.Melody
import Haskore.Music as M
import Haskore.Basic.Duration
import Haskore.Music.GeneralMIDI
import Helper
import Haskore.Basic.Pitch as Pit
import Prelude as P
import Data.Maybe

-- Canon a 2 "Crab Canon", from Musical Offering (1747), by J. S. Bach

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

convert::[Pit.T] -> [Haskore.Melody.T ()]
convert pits = insert 5 enr $ makeMelody pits durations

durations::[Haskore.Basic.Duration.T]
durations = P.replicate 10 qn ++ P.replicate 7 en ++ P.replicate 4 qn ++ P.replicate 64 sn ++ P.replicate 4 en

takeAfter::Int -> Int -> [a] -> [a]
takeAfter after n l = P.take n $ snd $ splitAt after l

original =  line $ convert pitches
retrograde = M.reverse original
together = original =:= retrograde

main = p together