module QuerendoInvenietis where

import Haskore.Melody
import Haskore.Music as M
import Haskore.Music.GeneralMIDI
import Haskore.Basic.Duration
import Helper
import Haskore.Basic.Pitch as Pit
import Prelude as P
import Data.Maybe

pitches::[Pit.T]
pitches = nTimes 2 mainPitches ++ subPitches ++ nTimes 2 mainPitches ++ subSubPitches

mainPitches = subPitches ++ toP 0 [B,Gs,B,F,Fs,G,As,B] ++ toP 1 [C,D,E,D,Cs,D,Cs]++[(0,B)]++toP 1 [D,Cs]
            ++toP 0 [B,As,B] ++ [(1,Cs)] ++ toP 0 [Fs,F,D,F,Fs,Gs,As,B,As]

subPitches= subSubPitches ++ toP 1 [B,A,G,Fs,E,D,Fs,E,Fs,E,D,Cs,D,E,Cs,D]

subSubPitches= [(0,B)] ++ toP 1 [C,D,Ds,E,F,Ds,G] ++ [(0,A)] ++ toP 1 [Fs,F,E,Ds,D,Cs] ++ toP 0 [B,As,Fs,B] ++ toP 1 [E,D]
                        ++ nTimes 4 (toP 1 [D,Cs]) ++ [(0,B),(1,Cs),(0,B)]

durs::[Haskore.Basic.Duration.T]
durs = [sn,sn] ++ P.replicate 4 en

--TODO: complete rest tuples
restTuple=[(8,rest qn),(70,rest en),(73,rest en),(76,rest en),(86,rest qn)]

testPlay = p $ line $ toNote en pitches

convert = zipWith (\a b -> note a b ())
