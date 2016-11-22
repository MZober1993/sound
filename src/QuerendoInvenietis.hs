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
pitches = firstPart ++ secondPart

firstPart = nTimes 2 mainPitches ++ subPitches

secondPart = nTimes 2 mainPitches ++ subSubPitches

mainPitches = subPitches ++ toP 0 [B,Gs,B,F,Fs,G,As,B] ++ toP 1 [C,D,E,D,Cs,D,Cs]++[(0,B)]++toP 1 [D,Cs]
            ++toP 0 [B,As,B] ++ [(1,Cs)] ++ toP 0 [Fs,F,D,F,Fs,Gs,As,B,As]

subPitches= subSubPitches ++ toP 1 [B,A,G,Fs,E,D,Fs,E,Fs,E,D,Cs,D,E,Cs,D]

subSubPitches= [(0,B)] ++ toP 1 [Cs,D,Ds,E,F,Fs,G] ++ [(0,A)] ++ toP 1 [Fs,F,E,Ds,D,Cs] ++ toP 0 [B,As,Fs,B] ++ toP 1 [E,D]
                        ++ nTimes 4 (toP 1 [D,Cs]) ++ [(0,B),(1,Cs),(0,B)]

original = line $ insertMore restTuple $ convert pitches durs

durs::[Haskore.Basic.Duration.T]
durs = firstDur ++ secondDur

firstDur = nTimes 2 mainDur ++ P.take (length subPitches) mainDur
secondDur = nTimes 2 mainDur ++ P.take (length subSubPitches) mainDur

mainDur = [sn,sn] ++ P.replicate 4 en ++ P.replicate 3 qn ++ [en,en,qn,en,en,qn] ++  P.replicate 5 en
               ++ P.replicate 8 (1/128) ++ P.replicate 22 sn ++ [en] ++ P.replicate 6 sn ++ [en]
               ++ P.replicate 6 sn ++ [en,sn,sn,en,en,qn,qn] ++ P.replicate 7 sn

restTuple = firstRest ++ (firstRest >>= (\(a,b)->[(a+length firstPart,b)]))

firstRest = mainRests ++ (mainRests >>= (\(a,b)->[(a+length mainPitches,b)]))
    ++ (subRests >>= (\(a,b)->[(a+2*length mainPitches,b)]))

mainRests = subRests ++ [(70,snr),(73,snr),(76,snr),(86,qnr)]

subRests = (8,enr) : zip [22 .. 29] (P.replicate 8 $ rest (1/64))

testPlay = p $ line $ toNote en pitches

convert = zipWith (\a b -> note a b ())
