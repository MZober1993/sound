module EHelper where

import Euterpea
import System.Process
import System.Exit

exportAndPlay name m = exportPerfAndPlay name $ perform $ toMusic1 m

exportPerfAndPlay name perf = do
    exportMidiFile name $ toMidi perf
    p name

makeMelody::[Dur] -> [Pitch] -> [Music Pitch]
makeMelody = zipWith note

melodyWithRest r p d = insertMore r $ makeMelody d p

toP :: a -> [b] -> [(b,a)]
toP octave = map (\c -> (c,octave))

---- main utils ----

insert::Int -> a -> [a] -> [a]
insert n new_element xs = let (ys,zs) = splitAt n xs in ys ++ [new_element] ++ zs

nTimes::Int->[a]->[a]
nTimes n l = applyNTimes n (++ l) []

insertMore:: [(Int,a)]->[a]->[a]
insertMore ls xs = foldl (flip $ uncurry insert) xs ls

{-
insertMore ls xs = case ls of
    y:ys -> uncurry insert y (insertMore ys xs)
    _ -> xs
    ==
insertMore ls xs = foldl (\a b -> uncurry insert b a) xs ls
    -}

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]

p::String -> IO ExitCode
p name = rawSystem "timidity" [name]
