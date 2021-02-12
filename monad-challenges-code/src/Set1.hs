{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- rand :: Seed -> (Integer, Seed)
-- mkSeed :: Integer -> Seed



fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
    where (a, aSeed) = rand $ mkSeed 1
          (b, bSeed) = rand aSeed
          (c, cSeed) = rand bSeed
          (d, dSeed) = rand cSeed
          (e, _)     = rand dSeed


type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter = generalA toLetter rand

randString3 :: String
randString3 = [a, b, c]
    where (a, aSeed) = randLetter $ mkSeed 1
          (b, bSeed) = randLetter aSeed
          (c, _)     = randLetter bSeed


randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA ((+1) . (*2)) rand

randTen :: Gen Integer
randTen = generalA (*10) rand



randPair :: Gen (Char, Integer)
randPair = generalB (,) randLetter rand

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair genA genB seed = ((a, b), next)
    where (a, next') = genA seed
          (b, next)  = genB next'

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f genA genB seed = (f a b, next)
    where (a, next') = genA seed
          (b, next)  = genB next'

generalPair2 = generalB (,)



repRandom :: [Gen a] -> Gen [a]
repRandom [] seed = ([], seed)
repRandom (gen:gens) seed = (x:rest, last)
    where (x, next) = gen seed
          (rest, last) = repRandom gens next


generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen seed = (f n, next)
    where (n, next) = gen seed


genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo genA f seed = f a next
    where (a, next) = genA seed

mkGen :: a -> Gen a
mkGen = (,)



generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` \a ->
                    gb `genTwo` \b ->
                        mkGen $ f a b

repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (ga:gas) = ga `genTwo` \a ->
                      repRandom2 gas `genTwo` \as ->
                      mkGen (a:as)
