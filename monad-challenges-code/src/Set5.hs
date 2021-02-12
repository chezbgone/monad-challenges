{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst $ runGen ga s

instance Monad Gen where
    genA >>= f = Gen { runGen = \seed -> let (a, next) = runGen genA seed
                                          in runGen (f a) next
                      }
    return a = Gen { runGen = (,) a }

makeRandom :: Gen Integer
makeRandom = Gen { runGen = rand }


fiveRands :: Gen [Integer]
fiveRands = do
    a <- makeRandom
    b <- makeRandom
    c <- makeRandom
    d <- makeRandom
    e <- makeRandom
    return [a, b, c, d, e]

randLetter :: Gen Char
randLetter = do
    a <- makeRandom
    return $ toLetter a

randString3 :: Gen String
randString3 = do
    a <- randLetter
    b <- randLetter
    c <- randLetter
    return [a, b, c]

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do
    a <- ga
    b <- gb
    return (a, b)




data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x == Just y   = x == y
    _ == _             = False

instance Monad Maybe where
    Nothing >>= _ = Nothing
    Just a >>= f = f a
    return = Just

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((a, b):abs) = if a == x
                           then Just b
                           else lookupMay x abs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a 0 = Nothing
divMay a b = Just $ a/b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ foldl1 max xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just $ foldl1 min xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key = do
    xs <- lookupMay key greekData
    ts <- tailMay xs
    m <- maximumMay ts
    h <- headMay xs
    divMay (fromIntegral m) (fromIntegral h)


addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData keyA keyB = do
    a <- lookupMay keyA salaryData
    b <- lookupMay keyB salaryData
    return $ a + b


tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
    ts <- tailMay xs
    return $ product ts

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
    ts <- tailMay xs
    return $ sum ts

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
    ts <- tailMay xs
    return $ foldl1 max ts


queryTests = [ queryGreek greekDataA "alpha" == Just 2.0
             , queryGreek greekDataA "beta" == Nothing
             , queryGreek greekDataA "gamma" == Just 3.3333333333333335
             , queryGreek greekDataA "delta" == Nothing
             , queryGreek greekDataA "zeta" == Nothing
             , queryGreek greekDataB "rho" == Nothing
             , queryGreek greekDataB "phi" == Just 0.24528301886792453
             , queryGreek greekDataB "chi" == Just 9.095238095238095
             , queryGreek greekDataB "psi" == Nothing
             , queryGreek greekDataB "omega" == Just 24.0
             ]




instance Monad [] where
    [] >>= f = []
    (x:xs) >>= f = f x ++ (xs >>= f)
    return x = [x]


allPairs :: [a] -> [b] -> [(a, b)]
allPairs as bs = do
    a <- as
    b <- bs
    return (a, b)

data Card = Card Int String

allCards :: [Int] -> [String] -> [Card]
allCards as bs = do
    a <- as
    b <- bs
    return $ Card a b

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = do
    a <- as
    b <- bs
    c <- cs
    return $ f a b c
