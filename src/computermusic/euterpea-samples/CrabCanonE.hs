module CrabCanonE where

import Euterpea
import EHelper
-- Canon a 2 "Crab Canon", from Musical Offering (1747), by J. S. Bach

pitches::[Pitch]
pitches = firstLine ++ secondLine ++ thirdLine

firstLine =
  toP 4 [C, Ef, G, Af] ++
  [(B,3)] ++ toP 4 [G, Fs, F, E, Ef, D, Df, C]

secondLine =
  toP 3 [B, G, B] ++
  toP 4 [ F, Ef, D, C, Ef, G, F, G] ++
  [(C,5)] ++
  toP 4 [G, Ef, D, Ef, F, G, A, B] ++
  [(C,5)] ++ toP 4 [Ef, F, G, Af, D, Ef, F, G, F, Ef, D]

thirdLine =
  toP 4 [Ef, F, G, Af, Bf, Af, G, F, G, Af, Bf] ++
  [(C,5), (Df,5)] ++
  toP 4 [Bf, Af, G, A, B] ++
  toP 5 [C, D, Ef, C] ++
  toP 4 [B, A, B] ++
  toP 5 [C, D, Ef, F, D] ++
  [(G,4)] ++
  toP 5 [D, C, D, Ef, F, Ef, D, C] ++ [(B,4)] ++ [(C,5)] ++ toP 4 [G, Ef, C]

convert::[Pitch] -> [Music Pitch]
convert pits = insert 5 enr $ makeMelody durations pits

durations::[Dur]
durations = replicate 10 qn ++ replicate 7 en ++ replicate 4 qn ++ replicate 64 sn ++ replicate 4 en

original =  line $ convert pitches
retrograde = retro original
together = scaleDurations (1/2) $ original :=: retro original

main = exportAndPlay "crabcanon.mid" together
