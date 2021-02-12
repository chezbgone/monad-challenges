{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude


class Monad m where
    bind   :: m a -> (a -> m b) -> m b
    return :: a -> m a


newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst $ runGen ga s

instance Monad Gen where
    bind genA f = Gen { runGen = \seed -> let (a, next) = runGen genA seed
                                          in runGen (f a) next
                      }
    return a = Gen { runGen = (,) a }


data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x == Just y   = x == y
    _ == _             = False

instance Monad Maybe where
    bind Nothing = const Nothing
    bind (Just a) = ($a)
    return = Just


instance Monad [] where
    bind [] f = []
    bind (a:as) f = f a ++ bind as f
    return a = [a]



(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join mma = bind mma id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf `bind` \f ->
           ma `bind` \a ->
            return $ f a
           

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = ma `bind` \a ->
                    sequence mas `bind` \as ->
                        return (a:as)

mmap :: Monad m => (a -> b) -> m a -> m b
mmap f ma = (return . f) =<< ma

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = f `mmap` ma `ap` mb
                

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = f `mmap` ma `ap` mb `ap` mc


-- Set 1
randGen :: Gen Integer
randGen = Gen {runGen = rand}

fiveRands :: [Integer]
fiveRands = fst $ sequence (replicate 5 randGen) `runGen` mkSeed 1

randLetter :: Gen Char
randLetter = randGen `bind` (return . toLetter)

randString3 :: String
randString3 = fst $ sequence (replicate 3 randLetter) `runGen` mkSeed 1

randEven :: Gen Integer
randEven = mmap (*2) randGen

randOdd :: Gen Integer
randOdd = mmap ((+1) . (*2)) randGen

randTen :: Gen Integer
randTen = mmap (*10) randGen


-- Set 2
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
queryGreek greekData key = lookupMay key greekData `bind` \xs ->
                           maximumMay =<< tailMay xs `bind` \m ->
                           headMay xs `bind` \h ->
                               divMay (fromIntegral m) (fromIntegral h)


addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData keyA keyB = lookupMay keyA salaryData `bind` \a ->
                                   lookupMay keyB salaryData `bind` \b ->
                                       return $ a + b


-- Set 3
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2
