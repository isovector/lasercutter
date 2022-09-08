{-# LANGUAGE StrictData #-}

module Lasercutter.Types where

import Control.Applicative
import Control.Selective
import Data.Monoid
import Data.Profunctor
import Witherable


------------------------------------------------------------------------------
-- | Lasercutter supports any inductive tree types, as witnessed by
-- 'getChildren'.
--
-- @since 0.1.0.0
class IsTree t where
  -- | Get all children of the current node.
  --
  -- @since 0.1.0.0
  getChildren :: t -> [t]


------------------------------------------------------------------------------
-- | A tree parser which runs all queries in a single pass. This is
-- accomplished via a free encoding of the applicative structure, which can be
-- arbitrarily reassociated for better performance.
--
-- @since 0.1.0.0
data Parser bc t a where
  -- | The free 'pure' constructor.
  --
  -- @since 0.1.0.0
  Pure       :: a -> Parser bc t a
  -- | The free 'liftA2' constructor. This is an inlining of Day convolution.
  --
  -- @since 0.1.0.0
  LiftA2     :: (b -> c -> a) -> Parser bc t b -> Parser bc t c -> Parser bc t a
  -- | Get the breadcrumbs at the current part of the tree.
  --
  -- @since 0.1.0.0
  GetCrumbs  :: Parser bc t bc
  -- | Run the given parser at every subtree which matches the given predicate.
  -- This is not recursive --- that is, a given subtree only runs the given
  -- parser once, not in all further matching subtrees.
  --
  -- @since 0.1.0.0
  Target     :: (t -> Bool) -> Parser bc t a -> Parser bc t [a]
  -- | Run the given parser on each child of the current node.
  --
  -- @since 0.1.0.0
  OnChildren :: Parser bc t a -> Parser bc t [a]
  -- | Get the current node.
  --
  -- @since 0.1.0.0
  Current    :: Parser bc t t
  -- | Swallow a parsed 'Maybe', failing the parser if it was 'Nothing'. Don't
  -- use this constructor explicitly; prefer 'expect' which maintains some
  -- invariants.
  --
  -- 'optional' is the inverse to this parser.
  --
  -- @since 0.1.0.0
  Expect     :: Parser bc t (Maybe a) -> Parser bc t a
  -- | Immediately fail a parse. Equivalent to @'Expect' ('pure' 'Nothing')@.
  --
  -- @since 0.1.0.0
  Fail       :: Parser bc t a
  deriving (Semigroup, Monoid) via (Ap (Parser bc t) a)

instance Show (Parser bc t a) where
  show (Pure _) = "(Pure _)"
  show (LiftA2 _ pa' pa_bctc) =
    "(LiftA2 _ " <> show pa' <> " " <> show pa_bctc <> ")"
  show GetCrumbs = "GetCrumbs"
  show (Target _ pa') = "(Target _ " <> show pa' <> "')"
  show (OnChildren pa') = "(OnChildren " <> show pa' <> ")"
  show Current = "Current"
  show (Expect pa') = "(Expect " <> show pa' <> ")"
  show Fail = "Fail"


instance Functor (Parser bc t) where
  fmap = liftA

instance Applicative (Parser bc t) where
  pure = Pure
  liftA2 f (Pure a) (Pure b) = Pure $ f a b
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b

instance Alternative (Parser bc t) where
  empty = Fail
  pa1 <|> pa2 =
    expect $ maybe <$> try pa2 <*> pure Just <*> try pa1

instance Selective (Parser bc t) where
  select = selectA

instance Filterable (Parser bc t) where
  catMaybes = Expect

instance Profunctor (Parser bc) where
  lmap = mapTree
  rmap = fmap


------------------------------------------------------------------------------
-- | Transform the type of tree that a 'Parser' operates over.
--
-- @since 0.1.0.0
mapTree :: (t -> t') -> Parser bc t' a -> Parser bc t a
mapTree _ (Pure a)         = Pure a
mapTree t (LiftA2 f pa pb) = LiftA2 f (mapTree t pa) (mapTree t pb)
mapTree _ GetCrumbs        = GetCrumbs
mapTree t (Target p pa)    = Target (p . t) $ mapTree t pa
mapTree t (OnChildren pa)  = OnChildren $ mapTree t pa
mapTree t Current          = fmap t Current
mapTree t (Expect pa)      = Expect $ mapTree t pa
mapTree _ Fail             = Fail


------------------------------------------------------------------------------
-- | A parser to run on children, and a subsequent continuation for how to
-- parse the parent.
--
-- @since 0.1.0.0
data Split bc t a where
  Split
      :: Parser bc t a
         -- ^ The parser to run on children.
      -> ([a] -> Parser bc t b)
         -- ^ Continuation for how to subsequently parse the current node.
      -> Split bc t b


------------------------------------------------------------------------------
-- | Swallow a parsed 'Maybe', failing the parser if it was 'Nothing'.
--
-- Use 'try' or 'optional' as the inverse to this parser.
--
-- @since 0.1.0.0
expect :: Parser bc t (Maybe a) -> Parser bc t a
expect (Pure Nothing)  = Fail
expect (Pure (Just a)) = Pure a
expect p               = Expect p


------------------------------------------------------------------------------
-- | Like 'optional', but slightly more efficient.
--
-- @since 0.1.0.0
try :: Parser bc t a -> Parser bc t (Maybe a)
try Fail           = pure Nothing
try (Expect p)     = p
try (LiftA2 f a b) = LiftA2 (liftA2 f) (try a) (try b)
try p              = fmap Just p

