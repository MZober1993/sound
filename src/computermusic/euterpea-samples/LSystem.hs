module LSystem where

import Euterpea
import Data.List hiding (transpose)
import System.Random
import Data.Maybe
import EHelper

data DetGrammar a = DetGrammar  a           --  start symbol
                                [(a,[a])]   --  productions
  deriving Show

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate (DetGrammar st ps) = iterate (concatMap f) [st]
            where f a =  fromMaybe [a] (lookup a ps)

redAlgae = DetGrammar 'a'
               [  ('a',"b|c"),   ('b',"b"),  ('c',"b|d"),
                  ('d',"e\\d"),  ('e',"f"),  ('f',"g"),
                  ('g',"h(a)"),  ('h',"h"),  ('|',"|"),
                  ('(',"("),     (')',")"),  ('/',"\\"),
                  ('\\',"/")
               ]

t n g = mapM_ putStrLn (take n (detGenerate g))

data Grammar a = Grammar  a          --  start sentence
                          (Rules a)  --  production rules
     deriving Show

data Rules a  =  Uni  [Rule a]
              |  Sto  [(Rule a, Prob)]
     deriving (Eq, Ord, Show)

data Rule a = Rule { lhs :: a, rhs :: a }
     deriving (Eq, Ord, Show)

type Prob = Double
type ReplFun a  = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand       = Double

gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed =
    let  Sto newRules  = toStoRules rules
         rands         = randomRs (0.0,1.0) (mkStdGen seed)
    in  if checkProbs newRules
        then generate f newRules (s,rands)
        else error "Stochastic rule-set is malformed."

toStoRules :: (Ord a, Eq a) => Rules a -> Rules a
toStoRules (Sto rs)  = Sto rs
toStoRules (Uni rs)  =
  let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
  in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)]
insertProb rules =  let prb = 1.0 / fromIntegral (length rules)
                    in zip rules (repeat prb)

checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = all checkSum (groupBy sameLHS (sort rs))

eps = 0.001

checkSum :: [(Rule a, Prob)] -> Bool
checkSum rules =  let mySum = sum (map snd rules)
                  in abs (1.0 - mySum) <= eps

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool
sameLHS (r1,f1) (r2,f2) = lhs r1 == lhs r2

generate ::  Eq a =>
             ReplFun a -> [(Rule a, Prob)] -> (a,[Rand]) -> [a]
generate f rules xs =
  let  newRules      =  map probDist (groupBy sameLHS rules)
       probDist rrs  =  let (rs,ps) = unzip rrs
                        in zip rs (tail (scanl (+) 0 ps))
  in map fst (iterate (f newRules) xs)

data LSys a  =  N a
             |  LSys a   :+   LSys a
             |  LSys a   :.   LSys a
             |  LSys a := LSys a -- self written
             |  Id
     deriving (Eq, Ord, Show)

replFun :: Eq a => ReplFun (LSys a)
replFun rules (s, rands) =
  case s of
    a :+ b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :+ b', rands'')
    a :. b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' :. b', rands'')
    a := b  ->  let  (a',rands')   = replFun rules (a, rands )
                     (b',rands'')  = replFun rules (b, rands')
                in (a' := b', rands'')
    Id      ->  (Id, rands)
    N x     ->  (getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand =
  let  loop ((r,p):rs)  = if rand <= p then rhs r else loop rs
       loop []          = error "getNewRHS anomaly"
  in case find (\ ((r,p):_) -> lhs r == ls) rrs of
        Just rs  -> loop rs
        Nothing  -> error "No rule match"

type IR a b = [(a, Music b -> Music b)]  --  ``interpetation rules''

interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b)  r m = interpret a r (interpret b r m)
interpret (a :+ b)  r m = interpret a r m :+: interpret b r m
interpret (a := b)  r m = interpret a r m :=: interpret b r m
interpret Id        r m = m
interpret (N x)     r m = case lookup x r of
                            Just f   -> f m
                            Nothing  -> error "No interpetation rule"

data LFun = Inc | Dec | Same
     deriving (Eq, Ord, Show)

ir :: IR LFun Pitch
ir = [ (Inc, transpose 1),
       (Dec, transpose (-1)),
       (Inc2, transpose 2),
       (Dec2, transpose (-2)),
       (Same, id)]

inc, dec, same :: LSys LFun
inc   = N Inc
dec   = N Dec
same  = N Same

sc = inc :+ dec
sp = inc := dec

r1a  = Rule inc (sc :. sc)
r1b  = Rule inc sc
r2a  = Rule dec (sc :. sc)
r2b  = Rule dec sc

r1b'  = Rule inc sp
r2a'  = Rule dec (sp :. sp)

r3a  = Rule same inc
r3b  = Rule same dec
r3c  = Rule same same

seqGrammar = Grammar same (Uni [r1b, r1a, r2b,r2a, r3a, r3c, r3b])
parGrammar = Grammar same (Uni [r1b, r1a, r2b,r2a, r2a', r3a, r3c, r3b])
parGrammar' = Grammar same (Uni [r1b, r1a, r2b,r2a, r2a',r1b', r3a, r3c, r3b])

genOnPos gr n = gen replFun gr 42 !! n
track gr n = instrument AcousticGrandPiano $ interpret (genOnPos gr n) ir (f 3 sn)
playLSys gr n = exportAndPlay "lsys.mid" $ track gr n