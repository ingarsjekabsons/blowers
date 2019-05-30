{-# LANGUAGE InstanceSigs #-}

module List where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup a => Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    (<>) Nil x = x
    (<>) x Nil = x
    (<>) (Cons a as) (Cons b bs) = Cons (a <> b) (as <> bs)

instance Monoid a => Monoid (List a) where
    mempty = Cons mempty Nil

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative List where
    pure x = Cons x Nil
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)
    (<*>) Nil _ = Nil




instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        return $ Cons a Nil

instance Eq a => EqProp (List a) where (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as


testList :: IO ()
testList = do
    quickBatch $ semigroup (undefined :: (List String, Int))
    quickBatch $ monoid (undefined :: List String)
    quickBatch $ functor (undefined :: List (String, String, Bool))
    quickBatch $ applicative (undefined :: List (String, String, Bool))
