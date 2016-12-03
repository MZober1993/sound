module EuterpeaSamples.QuaerendoInvenietisE where

import Euterpea
--import EuterpeaSamples.EHelper TODO: fix imports
--import Util.BasicHelper

-- Canon a 2 Quaerendo Invenietis, from Musical Offering, BMV 1079 49, by J. S. Bach
mainPitches,subPitches,subSubPitches :: [Pitch]
mainPitches = subPitches ++ toP 3 [B,Gs,B,F,Fs,Gs,As,B] ++ toP 4 [Cs,D,E,D,Cs,D,Cs] ++ [(B,1)]
            ++ toP 4 [D,Cs] ++ toP 3 [B,As,B] ++ [(Cs,4)] ++ toP 3 [Fs,F,D,F,Fs,Gs,As,B,As]

subPitches= subSubPitches ++ toP 4 [B,A,G,Fs,E,D,Fs,E,Fs,E,D,Cs,D,E,Cs,D]
-- TODO: find the error-pitch (to high)
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

inverted = transpose (-10) $ line $ wnr:[invert original] {-firstIMelody ++ secondIMelody
firstIMelody = nTimes 2 mainIMelody ++ subIMelody
secondIMelody = nTimes 2 mainIMelody ++ subSubIMelody

mainIMelody = buildMelody (changePitches invert mainPitches)
subIMelody = buildMelody (changePitches invert subPitches)
subSubIMelody = buildMelody (changePitches invert subSubPitches)-}

together = original :=: inverted

main = expSMidi "querendo-invenietis.mid" together


--------------------

expSMidi s m= exportMidiFile s $ toMidi $ perform $ toMusic1 m

makeMelody::[Dur] -> [Pitch] -> [Music Pitch]
makeMelody = zipWith note

melodyWithRest r p d = insertMore r $ makeMelody d p

toP :: a -> [b] -> [(b,a)]
toP octave = map (\c -> (c,octave))

insert::Int -> a -> [a] -> [a]
insert n new_element xs = let (ys,zs) = splitAt n xs in ys ++ [new_element] ++ zs

nTimes::Int->[a]->[a]
nTimes n l = applyNTimes n (++ l) []

insertMore:: [(Int,a)]->[a]->[a]
insertMore ls xs = foldl (flip $ uncurry insert) xs ls

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]


