module ExamplesE where

import EHelper
import Euterpea

simple1 = c 4 qn  :+: e 4 qn  :+: g 4 qn
simple2 = c 4 qn  :=: e 4 qn  :=: g 4 qn
simple3 = c 4 qn  :+: qnr :+: g 4 qn
simple4 = [(C,4),(E,4),(G,4)]
simple5 = note qn (C,4)
simple7 = map (note qn) simple4

simple1' = line $ map (\x -> x 4 qn ) [c, e, g]
simple2' = chord $ map (\x -> x 4 qn ) [c, e, g]

transpose1 = line $ map (\i-> transpose i $ c 4 qn )  [0 .. 8]
transpose2 = line $ map (`transpose` simple2')  [0 .. 8]

changeTempo1= tempo sn simple1
changeTempo2= chord [tempo sn simple1,simple2]

replicate1 = replicate 10 simple1
replicate2 = replicate 1 simple1

invert' = invert simple1'
retro' = retro simple1
retroInvert' = retroInvert simple1
invertRetro' = invertRetro simple1
-- example for reverse given in CrabCanon.hs
--         for invert  given in QuaerendoInventietis.hs
