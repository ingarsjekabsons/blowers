{-# LANGUAGE InstanceSigs #-}

module Validation where

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap :: (a -> b) -> Validation e a -> Validation e b
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e


instance Monoid e => Applicative (Validation e) where
    pure :: a -> Validation e a
    pure = Success
    (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
    (Success f) <*> (Success x) = Success (f x)
    (Success _) <*> (Failure e) = Failure e
    (Failure x) <*> (Failure y) = Failure (x <> y)
    (Failure x) <*> (Success _) = Failure x




instance (Eq e, Eq b) => EqProp (Validation e b) where (=-=) = eq

instance (Arbitrary e, Arbitrary b) => Arbitrary (Validation e b) where
    arbitrary = do
        e <- arbitrary
        b <- arbitrary
        frequency [(1, return $ Failure e), (1, return $ Success b)]


testValidation :: IO ()
testValidation = do
    quickBatch $ functor (undefined :: Validation String (String, String, Bool))
    quickBatch $ applicative (undefined :: (Validation String) (String, String, Bool))
