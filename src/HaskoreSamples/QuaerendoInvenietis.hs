module QuaerendoInvenietis where

import Haskore.Melody
import Haskore.Music as M
import Haskore.Music.GeneralMIDI
import Haskore.Basic.Duration
import Helper
import Haskore.Basic.Pitch as Pit
import Prelude as P
import Data.Maybe

-- Canon a 2 Quaerendo Invenietis, from Musical Offering, BMV 1079 49, by J. S. Bach

mainPitches = subPitches ++ toP 0 [B,Gs,B,F,Fs,Gs,As,B] ++ toP 1 [Cs,D,E,D,Cs,D,Cs]++[(0,B)]++toP 1 [D,Cs]
            ++toP 0 [B,As,B] ++ [(1,Cs)] ++ toP 0 [Fs,F,D,F,Fs,Gs,As,B,As]

subPitches= subSubPitches ++ toP 1 [B,A,G,Fs,E,D,Fs,E,Fs,E,D,Cs,D,E,Cs,D]

subSubPitches= [(0,B)] ++ toP 1 [Cs,D,Ds,E,F,Fs,G] ++ [(0,As)] ++ toP 1 [Fs,F,E,Ds,D,Cs] ++ toP 0 [B,As,Fs,B] ++ toP 1 [E,D]
                        ++ nTimes 4 (toP 1 [D,Cs]) ++ [(0,B),(1,Cs),(0,B)]

mainDur = [sn,sn] ++ P.replicate 4 en ++ P.replicate 3 qn ++ [en,en,qn,en,en,qn] ++  P.replicate 5 en ++ [qn]
               ++ P.replicate 8 (1/64) ++ P.replicate 22 sn ++ [en] ++ P.replicate 6 sn ++ [en]
               ++ P.replicate 6 sn ++ [en,sn,sn,en,en,qn,qn] ++ P.replicate 7 sn

restTuple = [(9,enr),(73,snr),(77,snr),(81,snr)]

buildMelody p = melodyWithRest restTuple p mainDur

original = line $ firstMelody ++ secondMelody
firstMelody = nTimes 2 mainMelody ++ subMelody
secondMelody = nTimes 2 mainMelody ++ subSubMelody

mainMelody = buildMelody mainPitches
subMelody = buildMelody subPitches
subSubMelody = buildMelody subSubPitches

inverted = M.transpose (-10) $ line $ [wnr] ++ firstIMelody ++ secondIMelody
firstIMelody = nTimes 2 mainIMelody ++ subIMelody
secondIMelody = nTimes 2 mainIMelody ++ subSubIMelody

mainIMelody = buildMelody (changePitches invert mainPitches)
subIMelody = buildMelody (changePitches invert subPitches)
subSubIMelody = buildMelody (changePitches invert subSubPitches)

together = original =:= inverted

main = p together