module TryPerform where

import Euterpea
import MoreMusic
import EHelper
import Perform

legato = Legato (11/10)
staccato = Staccato (5/10)
ssfMelody = Modify (Phrase [Dyn (StdLoudness PPP),Dyn (Crescendo 10), Tmp (Ritardando (5/10))])
    (line (m1 ++ m2 ++ m3 ++ m4))

m1 = [trilln 2 5 (bf 5 en), Modify (Phrase [Art staccato]) (line [ef 6 en, ef 5 en, ef 6 en])]
m2 = [Modify (Phrase [Art legato]) (line [bf 5 sn, c  6 sn, bf 5 sn, g  5 sn]),
	    Modify (Phrase [Art staccato])    (line [ef 5 en, bf 4 en])]
m3 = [Modify (Phrase [Art legato]) (line [ef 5 sn, f  5 sn, g  5 sn, af 5 sn]),
	    Modify (Phrase [Art staccato])    (line [bf 5 en, ef 6 en])]
m4 = [trill 2 tn (bf 5 qn), bf 5 sn, denr]
ssf = Modify (Instrument Oboe) ssfMelody

tryFancyAndPlay = exportPerfAndPlay "fancy.mid" tryFancy
tryDefaultAndPlay = exportPerfAndPlay "default.mid" tryDefault
mergeBothAndPlay = exportPerfAndPlay "both.mid" mergeBoth
-- the fancy player has a better StdLoudness scala and an interpretation of crescendo, ritardando...

tryFancy::Performance
tryFancy = hsomPerform defPMap fancyCon $ toMusic1 ssfMelody

tryDefault::Performance
tryDefault = hsomPerform defPMap defCon $ toMusic1 ssfMelody

mergeBoth::Performance
mergeBoth = merge tryDefault tryFancy
