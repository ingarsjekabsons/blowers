{-# language InstanceSigs #-}

module Either where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Eithers a b = Lefts a | Rights b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Eithers a b) where
    (<>) :: Eithers a b -> Eithers a b -> Eithers a b
    (<>) (Lefts a) (Lefts b) = Lefts (a <> b)
    (<>) (Rights a) (Rights b) = Rights (a <> b)
    (<>) (Lefts _) (Rights b) = Rights b
    (<>) (Rights a) (Lefts _) = Rights a

instance Functor (Eithers e) where
    fmap :: (a -> b) -> Eithers e a -> Eithers e b
    fmap f (Rights v) = Rights (f v)
    fmap _ (Lefts a) = Lefts a

instance Applicative (Eithers e) where
    pure :: a -> Eithers e a
    pure          = Rights
    (<*>) :: Eithers e (a -> b) -> Eithers e a -> Eithers e b
    (<*>) (Lefts e) _ = Lefts e
    (<*>) (Rights f) (Rights v) = Rights (f v)




instance (Arbitrary a, Arbitrary b) => Arbitrary (Eithers a b) where
    arbitrary = frequency [(1, Lefts <$> arbitrary), (1, Rights <$> arbitrary)]


instance (Eq a, Eq b) => EqProp (Eithers a b) where (=-=) = eq


testEither :: IO ()
testEither = do
    quickBatch $ semigroup (undefined :: (Eithers String String, Int))
    quickBatch $ functor (undefined :: (Eithers String) (String, String, Bool))
    quickBatch $ applicative (undefined :: (Eithers String) (String, String, Bool))
