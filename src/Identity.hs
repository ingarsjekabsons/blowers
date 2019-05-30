module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <>  b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity v) = Identity (f v) 


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq


testId :: IO ()
testId = do
    quickBatch $ semigroup (undefined :: (Identity String, Int))
    quickBatch $ monoid (undefined :: Identity String)
    quickBatch $ functor (undefined :: Identity (String, String, Bool))
    quickBatch $ applicative (undefined :: Identity (String, String, Bool))
