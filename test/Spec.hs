{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Int (Int8)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lasercutter
import Lasercutter.Types
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Foldable (traverse_)
import Data.Monoid (Any)


instance {-# OVERLAPPABLE #-} (CoArbitrary t, Arbitrary a, Arbitrary bc, CoArbitrary bc, CoArbitrary a) => Arbitrary (Parser bc t a) where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              -- , pure GetCrumbs
              , Project <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ LiftA2
                  <$> (arbitrary @(bc -> Int8 -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> (arbitrary @(Bool -> Bool -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> (arbitrary @(a -> a -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , expect <$> scale (subtract 1) arbitrary
               ] <> terminal
  shrink (Pure a)         = Fail : (fmap Pure $ shrink a)
  shrink GetCrumbs        = [Fail]
  shrink (LiftA2 _ _ _)   = [Fail]
  shrink (Target _ pa')   = Fail : (pure $ fmap pure pa')
  shrink (OnChildren pa') = Fail : (pure $ fmap pure pa')
  shrink (Try pa')        = Fail : (pure $ fmap pure pa')
  shrink (Project _)      = [Fail]
  shrink (Expect pa')     = Fail : (fmap expect $ shrink pa')
  shrink Fail = []


instance (CoArbitrary t, Arbitrary a, Arbitrary (Parser bc t a), CoArbitrary bc, CoArbitrary a, Arbitrary bc) => Arbitrary (Parser bc t [a]) where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              -- , pure GetCrumbs
              , Project <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ LiftA2
                  <$> arbitrary @(bc -> Int8 -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> arbitrary @(Bool -> Bool -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> arbitrary @(a -> a -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , Target <$> arbitrary <*> scale (subtract 1) arbitrary
              , OnChildren <$> scale (subtract 1) arbitrary
              , expect <$> scale (subtract 1) arbitrary
              ] <> terminal


instance (CoArbitrary t, Arbitrary a, Arbitrary (Parser bc t a), CoArbitrary bc, CoArbitrary a, Arbitrary bc) => Arbitrary (Parser bc t (Maybe a)) where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              , Project <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ LiftA2
                  <$> arbitrary @(bc -> Int8 -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> arbitrary @(Bool -> Bool -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , LiftA2
                  <$> arbitrary @(a -> a -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , tryP <$> scale (subtract 1) arbitrary
              , expect <$> scale (subtract 1) arbitrary
              ] <> terminal


data Four = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Arbitrary Four where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Four

data DebugTree
  = Leaf Int8
  | Branch Four [DebugTree]
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary DebugTree where
  arbitrary
    = let terminal = [Leaf <$> arbitrary]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ Branch <$> arbitrary <*> scale (flip div 2) arbitrary
              ] <> terminal

instance CoArbitrary DebugTree

instance IsTree DebugTree where
  getChildren (Leaf _) = []
  getChildren (Branch _ x) = x

instance EqProp Four

instance EqProp DebugTree

instance (EqProp a) => EqProp (Parser (Set Four) DebugTree a) where
  p1 =-= p2 = property $ do
    t <- arbitrary
    f <- arbitrary
    pure $ runParser f t p1 =-= runParser f t p2

instance EqProp Int8 where
  (=-=) = (===)


main :: IO ()
main = do
  quickBatch $ functor     $ undefined @_ @(Parser (Set Four) DebugTree (Int8, Int8, Int8))
  quickBatch $ applicative $ undefined @_ @(Parser (Set Four) DebugTree (Int8, Int8, Int8))
  quickBatch $ alternative $ undefined @_ @(Parser (Set Four) DebugTree Int8)
  quickBatch $ semigroup   $ undefined @_ @(Parser (Set Four) DebugTree Any, Int8)
  quickBatch $ monoid      $ undefined @_ @(Parser (Set Four) DebugTree Any)

  traverse_ quickCheck
    [ let a = Pure (-25)
          b = Fail @(Set Four) @DebugTree @Int8
          c = expect Fail
       in ((a <|> b) <|> c) =-= (a <|> (b <|> c))
    ]

