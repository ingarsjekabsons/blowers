module Constant where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Const a b = Const { getConst :: a } deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Const a b) where
    (<>) (Const a) (Const b) = Const (a <> b)

instance Monoid a => Monoid (Const a b) where
    mempty = Const mempty

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
    pure _ = Const mempty
    (<*>) (Const a) (Const b) = Const $ a <> b




instance (Arbitrary a) => Arbitrary (Const a b) where
    arbitrary = Const <$> arbitrary

instance Eq a => EqProp (Const a b) where (=-=) = eq


testStuff :: IO ()
testStuff = do
    quickBatch $ semigroup (undefined :: (Const String Bool, Int))
    quickBatch $ monoid (undefined :: Const String Bool)
    quickBatch $ functor (undefined :: (Const String) (String, String, Bool))
    quickBatch $ applicative (undefined :: (Const String) (String, String, Bool))
