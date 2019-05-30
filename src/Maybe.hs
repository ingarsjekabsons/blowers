{-# LANGUAGE InstanceSigs #-}
module Maybe where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data M a = N | J a deriving (Eq, Show)

instance Semigroup a => Semigroup (M a) where
    (<>) :: M a -> M a -> M a
    (<>) N (J x) = J x
    (<>) (J x) (J y) = J (x <> y)
    (<>) (J x) N = J x
    (<>) N N = N

instance Monoid a => Monoid (M a) where
    mempty :: M a
    mempty = J mempty


instance Functor M where
    fmap :: (a -> b) -> M a -> M b
    fmap f N = N
    fmap f (J a) = J (f a)


instance Applicative M where
    pure :: a -> M a
    pure = J

    (<*>) :: M (a -> b) -> M a -> M b
    (<*>) (J f) (J v) = J (f v)
    (<*>) N _ = N
    (<*>) (J f) N = N


instance Arbitrary a => Arbitrary (M a) where
    arbitrary = frequency [(1, return N), (1, J <$> arbitrary)]

instance Eq a => EqProp (M a) where (=-=) = eq


testMaybe :: IO ()
testMaybe = do
    quickBatch $ semigroup (undefined :: (Maybe String, Int))
    quickBatch $ monoid (undefined :: Maybe String)
    quickBatch $ functor (undefined :: Maybe (String, String, Bool))
    quickBatch $ applicative (undefined :: Maybe (String, String, Bool))
