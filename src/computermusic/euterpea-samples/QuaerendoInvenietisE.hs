module EuterpeaSamples.QuaerendoInvenietisE where

import Euterpea
import EHelper

-- Canon a 2 Quaerendo Invenietis, from Musical Offering, BMV 1079 49, by J. S. Bach

mainPitches,subPitches,subSubPitches :: [Pitch]
mainPitches = subPitches ++ toP 3 [B,Gs,B,F,Fs,Gs,As,B] ++ toP 4 [Cs,D,E,D,Cs,D,Cs] ++ [(B,3)]
            ++ toP 4 [D,Cs] ++ toP 3 [B,As,B] ++ [(Cs,4)] ++ toP 3 [Fs,F,D,F,Fs,Gs,As,B,As]

subPitches= subSubPitches ++ toP 4 [B,A,G,Fs,E,D,Fs,E,Fs,E,D,Cs,D,E,Cs,D]

subSubPitches= [(B,3)] ++ toP 4 [Cs,D,Ds,E,F,Fs,G] ++ [(As,3)] ++ toP 4 [Fs,F,E,Ds,D,Cs] ++ toP 3 [B,As,Fs,B]
                        ++ toP 4 [E,D] ++ nTimes 4 (toP 4 [D,Cs]) ++ [(B,3),(Cs,4),(B,3)]

mainDur = [sn,sn] ++ replicate 4 en ++ replicate 3 qn ++ [en,en,qn,en,en,qn] ++  replicate 5 en ++ [qn]
               ++ replicate 8 (1/64) ++ replicate 22 sn ++ [en] ++ replicate 6 sn ++ [en]
               ++ replicate 6 sn ++ [en,sn,sn,en,en,qn,qn] ++ replicate 7 sn

restTuple = [(9,enr),(73,snr),(77,snr),(81,snr)]

buildMelody p = melodyWithRest restTuple p mainDur

original = line $ firstMelody ++ secondMelody
firstMelody = nTimes 2 mainMelody ++ subMelody
secondMelody = nTimes 2 mainMelody ++ subSubMelody

mainMelody = buildMelody mainPitches
subMelody = buildMelody subPitches
subSubMelody = buildMelody subSubPitches

inverted = transpose (-10) $ line $ wnr:[invert original]

together = scaleDurations (1/2) $ original :=: inverted

main = exportAndPlay "quaerendo-invenietis.mid" together
