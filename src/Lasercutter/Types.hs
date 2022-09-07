{-# LANGUAGE StrictData #-}

module Lasercutter.Types where

import Control.Applicative
import Debug.RecoverRTTI (anythingToString)
import Witherable
import Data.Monoid


class IsTree t where
  getChildren :: t -> [t]


data Parser bc t a where
  Pure       :: a -> Parser bc t a
  GetCrumbs  :: Parser bc t bc
  LiftA2     :: (b -> c -> a) -> Parser bc t b -> Parser bc t c -> Parser bc t a
  Target     :: (t -> Bool) -> Parser bc t a -> Parser bc t [a]
  OnChildren :: Parser bc t a -> Parser bc t [a]
  Try        :: Parser bc t a -> Parser bc t (Maybe a)
  Project    :: (t -> a) -> Parser bc t a
  Expect     :: Parser bc t (Maybe a) -> Parser bc t a
  Fail       :: Parser bc t a
  deriving (Semigroup, Monoid) via (Ap (Parser bc t) a)


instance Show (Parser bc t a) where
  show = anythingToString

instance Functor (Parser bc t) where
  fmap = liftA

instance Applicative (Parser bc t) where
  pure = Pure
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b


instance Filterable (Parser bc t) where
  catMaybes = Expect


instance Alternative (Parser bc t) where
  empty = Fail
  pa1 <|> pa2 = expect $ maybe <$> tryP pa2 <*> pure Just <*> tryP pa1

data Bind bc t a where
  Bind :: Parser bc t a -> ([a] -> Parser bc t b) -> Bind bc t b


expect :: Parser bc t (Maybe a) -> Parser bc t a
expect (Try p) = p
expect Fail    = Fail
expect p       = Expect p


tryP :: Parser bc t a -> Parser bc t (Maybe a)
tryP Fail       = pure Nothing
tryP (Expect p) = p
tryP p          = fmap Just p


