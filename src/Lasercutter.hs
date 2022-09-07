module Lasercutter
  ( Parser
  , runParser

  -- * Combinators
  , expect
  , module Lasercutter

  -- * Machinery
  , IsTree (..)

  -- * Reexports
  , liftA2
  , Alternative(..)
  , optional
  , guard
  , asum
  , Filterable (..)
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Bool (bool)
import Data.Foldable (asum)
import Data.Maybe (listToMaybe, isJust)
import Lasercutter.Internal
import Lasercutter.Types
import Witherable (Filterable (..))


onChildren :: Parser bc t a -> Parser bc t [a]
onChildren = OnChildren


target :: (t -> Bool) -> Parser bc t a -> Parser bc t [a]
target = Target


breadcrumbs :: Parser bc t bc
breadcrumbs = GetCrumbs


onBreadcrumbs :: (bc -> a) -> Parser bc t a
onBreadcrumbs f = fmap f breadcrumbs


onSingleChild :: Parser bc t a -> Parser bc t (Maybe a)
onSingleChild = fmap listToMaybe . onChildren


onSelf :: (t -> a) -> Parser bc t a
onSelf = Project


self :: Parser bc t t
self = onSelf id


one :: Parser bc t [a] -> Parser bc t a
one = expect . fmap listToMaybe


ifThenElse
    :: Parser bc t Bool
    -> Parser bc t a
    -> Parser bc t a
    -> Parser bc t a
ifThenElse b tr fl
  = bool <$> fl <*> tr <*> b

when
    :: Parser bc t Bool
    -> Parser bc t a
    -> Parser bc t a
when b tr = expect $ ifThenElse b (fmap Just tr) $ pure Nothing


ifNode
    :: (t -> Bool)
    -- ^
    -> Parser bc t a
    -> Parser bc t a
    -> Parser bc t a
ifNode f tr fl = ifThenElse (onSelf f) tr fl


whenNode :: (t -> Bool) -> Parser bc t a -> Parser bc t a
whenNode = when . onSelf


targetMap :: (t -> Maybe a) -> Parser bc t [a]
targetMap f = fmap catMaybes $ target (isJust  . f) $ onSelf f

