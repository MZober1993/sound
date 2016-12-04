module EuterpeaSamples.Perform where

import Euterpea
import Data.List hiding (transpose)


eventTest = MEvent 0 Cello 27 (1/4) 50 [ ]
test = perform $ toMusic1 $ c 1 sn