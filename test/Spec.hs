{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Selective
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Int (Int8)
import Data.Monoid (Any)
import Data.Set (Set)
import Debug.RecoverRTTI (anythingToString)
import GHC.Generics (Generic)
import Lasercutter
import Lasercutter.Types
import Test.QuickCheck
import Test.QuickCheck.Checkers hiding (Test)
import Test.QuickCheck.Classes


type Test = Parser (Set Four) DebugTree

instance Show (Parser bc t a) where
  show = anythingToString


main :: IO ()
main = do
  traverse_ quickCheck
    [ -- expect is a left identity to optional
      property $ expect . optional =-= id @(Test Int8)

    , -- optional is a left identity to expect
      property $ optional . expect =-= id @(Test (Maybe Int8))

    , -- expect nothing is equivalent to fail
      property $ expect (pure Nothing) =-= (Fail :: Test Int8)

      -- expect distributes over liftA2
    , property $ \(f :: Int8 -> Four -> Bool) a b ->
        expect (liftA2 (liftA2 f) a b) =-= liftA2 @Test f (expect a) (expect b)

      -- optional equivalent to try
    , property $ optional @(Test) @Int8 =-= try
    ]

  quickBatch $ functor     $ undefined @_ @(Test (Int8, Int8, Int8))
  quickBatch $ applicative $ undefined @_ @(Test (Int8, Int8, Int8))
  quickBatch $ selective   $ undefined @_ @(Test (Int8, Int8, Int8))
  quickBatch $ alternative $ undefined @_ @(Test Int8)
  quickBatch $ semigroup   $ undefined @_ @(Test Any, Int8)
  quickBatch $ monoid      $ undefined @_ @(Test Any)


------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-}
    ( CoArbitrary t
    , Arbitrary a
    , Arbitrary bc
    , CoArbitrary bc
    , CoArbitrary a
    ) =>
    Arbitrary (Parser bc t a)
      where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ liftA2
                  <$> (arbitrary @(bc -> Int8 -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> (arbitrary @(Bool -> Bool -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> (arbitrary @(a -> a -> a))
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , fmap <$> arbitrary <*> pure Current
              , expect <$> scale (subtract 1) arbitrary
               ] <> terminal
  shrink (Pure a)         = Fail : (fmap Pure $ shrink a)
  shrink GetCrumbs        = [Fail]
  shrink (LiftA2 _ _ _)   = [Fail]
  shrink (Target _ pa')   = Fail : (pure $ fmap pure pa')
  shrink (OnChildren pa') = Fail : (pure $ fmap pure pa')
  shrink Current          = [Fail]
  shrink (Expect pa')     = Fail : (fmap expect $ shrink pa')
  shrink Fail = []


instance
    ( CoArbitrary t
    , Arbitrary a
    , Arbitrary (Parser bc t a)
    , CoArbitrary bc
    , CoArbitrary a
    , Arbitrary bc
    ) =>
    Arbitrary (Parser bc t [a])
      where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ liftA2
                  <$> arbitrary @(bc -> Int8 -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> arbitrary @(Bool -> Bool -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> arbitrary @(a -> a -> [a])
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , fmap <$> arbitrary <*> pure Current
              , Target <$> arbitrary <*> scale (subtract 1) arbitrary
              , OnChildren <$> scale (subtract 1) arbitrary
              , expect <$> scale (subtract 1) arbitrary
              ] <> terminal
  shrink (Pure a)         = Fail : (fmap Pure $ shrink a)
  shrink Current          = [Fail]
  shrink GetCrumbs        = [Fail]
  shrink (LiftA2 _ _ _)   = [Fail]
  shrink (Target _ pa')   = Fail : (pure $ fmap pure pa')
  shrink (OnChildren pa') = Fail : (pure $ fmap pure pa')
  shrink (Expect pa')     = Fail : (fmap expect $ shrink pa')
  shrink Fail = []


instance
    ( CoArbitrary t
    , Arbitrary a
    , Arbitrary (Parser bc t a)
    , CoArbitrary bc
    , CoArbitrary a
    , Arbitrary bc
    ) =>
    Arbitrary (Parser bc t (Maybe a))
      where
  arbitrary
    = let terminal
            = [ Pure <$> arbitrary
              , pure Fail
              ]
      in sized $ \ n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ liftA2
                  <$> arbitrary @(bc -> Int8 -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> arbitrary @(Bool -> Bool -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , liftA2
                  <$> arbitrary @(a -> a -> Maybe a)
                  <*> scale (flip div 2) arbitrary
                  <*> scale (flip div 2) arbitrary
              , fmap <$> arbitrary <*> pure Current
              , try <$> scale (subtract 1) arbitrary
              , expect <$> scale (subtract 1) arbitrary
              ] <> terminal
  shrink (Pure a)         = Fail : (fmap Pure $ shrink a)
  shrink GetCrumbs        = [Fail]
  shrink Current          = [Fail]
  shrink (LiftA2 _ _ _)   = [Fail]
  shrink (Expect pa')     = Fail : (fmap expect $ shrink pa')
  shrink Fail = []


------------------------------------------------------------------------------

data Four = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Arbitrary Four where
  arbitrary = elements $ enumFromTo minBound maxBound

instance CoArbitrary Four

instance EqProp Four


------------------------------------------------------------------------------

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

instance EqProp DebugTree

instance IsTree DebugTree where
  getChildren (Leaf _) = []
  getChildren (Branch _ x) = x


------------------------------------------------------------------------------

instance (EqProp a) => EqProp (Parser (Set Four) DebugTree a) where
  p1 =-= p2 = property $ do
    t <- arbitrary
    f <- arbitrary
    pure $ runParser f t p1 =-= runParser f t p2


------------------------------------------------------------------------------

instance EqProp Int8 where
  (=-=) = (===)


------------------------------------------------------------------------------

selective :: forall m a b c.
               ( Selective m
               , Arbitrary a, Arbitrary b
               , Arbitrary (m (Either a a)), Show (m (Either a a))
               , Arbitrary (m (Either a b)), Show (m (Either a b))
               , Arbitrary (m (Either c (a -> b))), Show (m (Either c (a -> b)))
               , Arbitrary (m (a -> b)), Show (m (a -> b))
               , Arbitrary (m (c -> a -> b)), Show (m (c -> a -> b))
               , Show a, Show b
               , EqProp (m a), EqProp (m b)
               ) =>
               m (a,b,c) -> TestBatch
selective = const ( "selective"
                  , [ ("identity"    , property identityP)
                    , ("distributivity" , property distributivityP)
                    , ("associativity", property associativityP)
                    ]
                  )
 where
   identityP     :: m (Either a a) -> Property
   distributivityP  :: Either a b -> m (a -> b) -> m (a -> b) -> Property
   associativityP :: m (Either a b) -> m (Either c (a -> b)) -> m (c -> a -> b) -> Property

   identityP x = (x <*? pure id) =-= fmap (either id id) x
   distributivityP x y z = (pure x <*? (y *> z)) =-= ((pure x <*? y) *> (pure x <*? z))
   associativityP x y z = (x <*? (y <*? z)) =-= ((fmap Right <$> x) <*? (g <$> y) <*? (uncurry <$> z))
    where
     g y' a = bimap (,a) ($a) y'

