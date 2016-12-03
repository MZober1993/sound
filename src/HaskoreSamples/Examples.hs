module HaskoreSamples.Examples where

import HaskoreSamples.Helper
import Haskore.Interface.MIDI.Render as Render
import Haskore.Music.GeneralMIDI
import Haskore.Music
import Haskore.Melody
import qualified Medium
import Haskore.Basic.Pitch as Pi hiding (transpose)
import Prelude as P hiding (replicate,reverse)
import Medium.Controlled.List

simple1 = c 1 qn () +:+ e 1 qn () +:+ g 1 qn ()
simple2 = c 1 qn () =:= e 1 qn () =:= g 1 qn ()
simple3 = c 1 qn () +:+ qnr +:+ g 1 qn ()
simple4 = [(1,C),(1,E),(1,G)]
simple5 = note (1,C) qn ()
simple6 = note' C 1 qn ()
simple7 = map (\i->note i qn ()) simple4

simple1' = line $ map (\x -> x 1 qn ()) [c, e, g]
simple2' = chord $ map (\x -> x 1 qn ()) [c, e, g]
simple4' = toP 1 [C,E,G] -- self-written in Helper
simple5' = toNote qn $ toP 1 [C,E,G] -- self-written in Helper

transpose1 = line $ map (\i-> transpose i $ c 1 qn ())  [0 .. 8]
transpose2 = line $ map (`transpose` simple2')  [0 .. 8]

changeTempo1= changeTempo sn simple1
changeTempo2= chord [changeTempo sn simple1,simple2]

replicate1 = replicate 10 simple1
replicate2 = replicate 1 simple1

invert1 = invert $ toTuple $ toP 1 [C,Cs,D,Ds,E,Es,F,Fs,G,Gs,A,As,B,Bs]
reverse1 = reverse simple1
reverse2 = reverse changeTempo2
-- example for reverse given in CrabCanon.hs
--         for invert  given in QuaerendoInventietis.hs
