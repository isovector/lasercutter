module Lasercutter
  ( -- * Core types
    Parser
  , runParser
  , IsTree (..)

  -- * Primitive parsers
  , proj
  , self

  -- * Setting expectations
  , expect
  , one

  -- * Traversing trees
  , onChildren
  , onSingleChild
  , target
  , targetMap

  -- * Conditional parsing
  , empty
  , (<|>)
  , try
  , optional
  , when
  , whenNode
  , ifS
  , ifNode

  -- * Breadcrumbs
  , breadcrumbs
  , onBreadcrumbs



  -- * Re-exports
  -- | The 'Parser' is an instance of all of the following classes, and thus
  -- all of these methods are available on 'Parser's.

  -- ** 'Applicative'
  , liftA2

  -- ** 'Alternative'
  , guard
  , asum

  -- ** 'Filterable'
  , mapMaybe
  , catMaybes
  , filter

  -- ** 'Selective'
  , select
  , (<*?)
  , branch
  , fromMaybeS
  , orElse
  , andAlso
  , (<||>)
  , (<&&>)
  , foldS
  , anyS
  , allS
  , bindS
  ) where

import Control.Applicative
import Control.Monad (guard)
import Control.Selective
import Data.Foldable (asum)
import Data.Maybe (listToMaybe, isJust)
import Lasercutter.Internal
import Lasercutter.Types
import Prelude hiding (filter)
import Witherable (Filterable (..))


------------------------------------------------------------------------------
-- | Project a value out of the current node. This is the main way to build
-- primitive parsers.
proj :: (t -> a) -> Parser bc t a
proj = Project


------------------------------------------------------------------------------
-- | Run the given parser on every immediate child of the current node.
onChildren :: Parser bc t a -> Parser bc t [a]
onChildren = OnChildren


------------------------------------------------------------------------------
-- | Run the given parser on every predicate-satisfying subtree of the current
-- node. This combinator is not recursive --- that is, if the predicate
-- is satisfied by both a node and its descendent, the descendent _will not_
-- receive the parser.
target :: (t -> Bool) -> Parser bc t a -> Parser bc t [a]
target = Target


------------------------------------------------------------------------------
-- | Get the breadcrumbs at the current node. This is useful for refining the
-- coarse-grained matches of 'target' by restricting matches to certain
-- subtrees.
breadcrumbs :: Parser bc t bc
breadcrumbs = GetCrumbs


------------------------------------------------------------------------------
-- | Get a value computed on the current breadcrumbs.
--
-- * @'onBreadcrumbs' f = 'fmap' f 'breadcrumbs'@
onBreadcrumbs :: (bc -> a) -> Parser bc t a
onBreadcrumbs f = fmap f breadcrumbs


------------------------------------------------------------------------------
-- | Run a parser on the immediate children of the current node, returning the
-- first success.
--
-- * @'onSingleChild' = 'fmap' 'listToMaybe' . 'onChildren'@
onSingleChild :: Parser bc t a -> Parser bc t (Maybe a)
onSingleChild = fmap listToMaybe . onChildren


------------------------------------------------------------------------------
-- | Get the current node.
--
-- * @'self' = 'proj' id@
self :: Parser bc t t
self = proj id


------------------------------------------------------------------------------
-- | Get the first result of a list of results, failing if there are none.
--
-- * @'one' = 'expect' . 'fmap' 'listToMaybe'@
one :: Parser bc t [a] -> Parser bc t a
one = expect . fmap listToMaybe


------------------------------------------------------------------------------
-- | @'when' pc pa@ returns @pa@ when @pc@ evaluates to 'True', failing
-- otherwise.
when
    :: Parser bc t Bool
    -> Parser bc t a
    -> Parser bc t a
when b tr = expect $ ifS b (fmap Just tr) $ pure Nothing


------------------------------------------------------------------------------
-- | @'ifNode' f pt pf@ runs @pt@ when @f@ evaluates to 'True' on the
-- current node, running @pf@ otherwise.
--
-- * @'ifNode' f tr fl = 'ifS' ('proj' f) tr fl@
ifNode
    :: (t -> Bool)
    -> Parser bc t a
    -> Parser bc t a
    -> Parser bc t a
ifNode f tr fl = ifS (proj f) tr fl


------------------------------------------------------------------------------
-- | @'whenNode' f pa@ returns @pa@ when @pc@ evaluates to 'True' on the
-- current node, failing otherwise.
--
-- * @'whenNode' = 'when' . 'proj'@
whenNode :: (t -> Bool) -> Parser bc t a -> Parser bc t a
whenNode = when . proj


------------------------------------------------------------------------------
-- | Run the given function on every subtree, accumulating those which return
-- 'Just'.
targetMap :: (t -> Maybe a) -> Parser bc t [a]
targetMap f = fmap catMaybes $ target (isJust  . f) $ proj f

