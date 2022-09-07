module Lasercutter
  ( -- * Core types
    Parser
  , runParser
  , IsTree (..)

  -- * Building parsers
  -- ** Primitives
  , self
  , proj

  -- ** Controlling failure
  , expect
  , one
  , try
  , empty
  , (<|>)

  -- ** Traversing trees
  , onChildren
  , onSingleChild
  , target
  , targetMap

  -- ** Conditional parsing
  , when
  , whenNode
  , ifS
  , ifNode

  -- ** Breadcrumbs
  -- | All parsers support a notion of *breadcrumbs* --- a monoid that gets
  -- accumulated along subtrees. Callers to 'runParser' can choose
  -- a *summarization* function which describes how to generate the breadcrumb
  -- monoid from the current node.
  --
  -- Breadcrumbs are often used to refine the results of 'target', which has no
  -- notion of history, and thus can be too coarse for many position-depending
  -- parsing tasks.
  , breadcrumbs
  , onBreadcrumbs
  , mapBreadcrumbs

  -- * Re-exports
  -- | The 'Parser' is an instance of all of the following classes, and thus
  -- all of these methods are available on 'Parser's.

  -- ** 'Profunctor'
  -- | Even though 'Parser's are contravariant in their breadcrumbs and
  -- tree type, this instance targets only the tree. Use 'mapBreadcrumbs' to
  -- modify the breadcrumbs.
  , dimap
  , rmap
  , lmap

  -- ** 'Applicative'
  , liftA2

  -- ** 'Alternative'
  , optional
  , guard
  , asum

  -- ** 'Filterable'
  , mapMaybe
  , catMaybes

  -- ** 'Selective'
  -- | 'Parser's are boring 'Selective' functors that are unfortunately unable
  -- to elide any effects. Nevertheless, the 'Selective' API is often quite
  -- useful for everyday parsing tasks.
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
import Data.Profunctor


------------------------------------------------------------------------------
-- | Project a value out of the current node. This is the main way to build
-- primitive parsers.
--
-- * @'proj' f = 'fmap' f 'self'@
proj :: (t -> a) -> Parser bc t a
proj f = fmap f self


------------------------------------------------------------------------------
-- | Run the given parser on every immediate child of the current node.
onChildren :: Parser bc t a -> Parser bc t [a]
onChildren = OnChildren


------------------------------------------------------------------------------
-- | Run the given parser on every predicate-satisfying subtree of the current
-- node. This combinator is not recursive --- that is, if the predicate
-- is satisfied by both a node and its descendent, the descendent *will not*
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
self :: Parser bc t t
self = Current


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

