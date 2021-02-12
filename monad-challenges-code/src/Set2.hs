{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x == Just y   = x == y
    _ == _             = False



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
queryGreek greekData key =
    case lookupMay key greekData of
      Nothing -> Nothing
      Just xs -> case tailMay xs of
                   Nothing -> Nothing
                   Just ts -> case maximumMay ts of
                                Nothing -> Nothing
                                Just m -> case headMay xs of
                                            Nothing -> Nothing
                                            Just h -> divMay (fromIntegral m) (fromIntegral h)

                               
greekTests f = [ f greekDataA "alpha" == Just 2.0
               , f greekDataA "beta" == Nothing
               , f greekDataA "gamma" == Just 3.3333333333333335
               , f greekDataA "delta" == Nothing
               , f greekDataA "zeta" == Nothing
               , f greekDataB "rho" == Nothing
               , f greekDataB "phi" == Just 0.24528301886792453
               , f greekDataB "chi" == Just 9.095238095238095
               , f greekDataB "psi" == Nothing
               , f greekDataB "omega" == Just 24.0
               ]


chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing  = Nothing
chain f (Just a) = f a


link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData key = lookupMay key greekData `link` \xs ->
                            maximumMay `chain` tailMay xs `link` \m ->
                            headMay xs `link` \h ->
                            divMay (fromIntegral m) (fromIntegral h)




addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaryData a b = lookupMay a salaryData `link` \salaryA ->
                             lookupMay b salaryData `link` \salaryB ->
                                 mkMaybe $ salaryA + salaryB

yLink :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
yLink f ma mb = ma `link` \a ->
                mb `link` \b ->
                    mkMaybe $ f a b

mkMaybe :: a -> Maybe a
mkMaybe = Just




tailProd :: Num a => [a] -> Maybe a
tailProd xs = transMaybe product (tailMay xs)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = transMaybe sum (tailMay xs)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` (mkMaybe . f)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = combine $ transMaybe maximumMay (tailMay xs)

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine $ transMaybe minimumMay (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just a) = a
