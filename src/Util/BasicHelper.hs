module Util.BasicHelper where

insert::Int -> a -> [a] -> [a]
insert n new_element xs = let (ys,zs) = splitAt n xs in ys ++ [new_element] ++ zs

nTimes::Int->[a]->[a]
nTimes n l = applyNTimes n (++ l) []

insertMore:: [(Int,a)]->[a]->[a]
insertMore ls xs = foldl (flip $ uncurry insert) xs ls

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]