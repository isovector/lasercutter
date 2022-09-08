module Lasercutter.Internal where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Lasercutter.Types


------------------------------------------------------------------------------
-- | Split a parser into a parser to run on the node's children, and how to
-- reassemble those pieces into a parser for the current node.
--
-- @since 0.1.0.0
split :: bc -> Parser bc t a -> t -> Split bc t a
split _ (Pure a) _         = ignoreChildren $ pure a
split cr GetCrumbs _       = ignoreChildren $ pure cr
split cr (LiftA2 f l r) tt =
  case (split cr l tt, split cr r tt) of
    (Split l' kl, Split r' kr) ->
      Split (LiftA2 (,) l' r') $ \(unzip -> (lcs, rcs)) ->
        LiftA2 f (kl lcs) (kr rcs)
split cr p0@(Target se pa) tt
  | se tt
  = continue (fmap pure) $ split cr pa tt
  | otherwise
  = Split p0 $ pure . join
split cr (Expect pa) tt    = continue expect $ split cr pa tt
split _ (OnChildren pa) _ = Split pa pure
split _ Current tt         = ignoreChildren $ pure tt
split _ Fail _             = Split Fail $ const $ Fail


------------------------------------------------------------------------------
-- | There is no work to do for the children, so ignore them.
--
-- @since 0.1.0.0
ignoreChildren :: Parser bc t b -> Split bc t b
ignoreChildren = Split (pure ()) . const


------------------------------------------------------------------------------
-- | Append a continuation after a 'Split'.
--
-- @since 0.1.0.0
continue :: (Parser bc t a -> Parser bc t b) -> Split bc t a -> Split bc t b
continue f (Split p k) = Split p $ f . k


------------------------------------------------------------------------------
-- | Parse the current node by splitting the parser, accumulating the results
-- of each child, and then running the continuation.
--
-- @since 0.1.0.0
parseNode
    :: (Semigroup bc, IsTree t)
    => (t -> bc)
    -> bc
    -> Parser bc t a
    -> t
    -> Parser bc t a
parseNode summarize cr p tt =
  case split cr' p tt of
    Split (Pure a) k -> k [a]
    Split pa k -> k $ parseChildren summarize cr' pa $ getChildren tt
  where
    cr' = summarize tt <> cr


------------------------------------------------------------------------------
-- | Run a parser on each child, accumulating the results.
--
-- @since 0.1.0.0
parseChildren
    :: (IsTree t, Semigroup bc)
    => (t -> bc)
    -> bc
    -> Parser bc t a
    -> [t]
    -> [a]
parseChildren summarize cr pa =
  mapMaybe $ getResult . parseNode summarize cr pa


------------------------------------------------------------------------------
-- | Extract a value from a parser. The way the applicative evaluates,
-- all "combinator" effects are guaranteed to have been run by the time this
-- function gets called.
--
-- @since 0.1.0.0
getResult :: Parser bc t a -> Maybe a
getResult (Pure a)       = pure a
getResult (LiftA2 f a b) = liftA2 f (getResult a) (getResult b)
getResult (Expect pa)    = join $ getResult pa
getResult Fail           = Nothing
getResult GetCrumbs      = error "getResult: impossible"
getResult (Target _ _)   = error "getResult: impossible"
getResult (OnChildren _) = error "getResult: impossible"
getResult Current        = error "getResult: impossible"


------------------------------------------------------------------------------
-- | Run a parser over a tree in a single pass.
--
-- @since 0.1.0.0
runParser
    :: (Monoid bc, IsTree t)
    => (t -> bc)
       -- ^ A means of summarizing the current node for tracking breadcrumbs.
       -- If you don't need breadcrumbs, use @'const' ()@.
    -> t
       -- ^ The tree to parse.
    -> Parser bc t a
       -- ^ How to parse the tree.
    -> Maybe a
runParser summarize tt =
  getResult . flip (parseNode summarize mempty) tt


------------------------------------------------------------------------------
-- | Transformer the breadcrumbs of a 'Parser'.
--
-- @since 0.1.0.0
mapBreadcrumbs :: (bc' -> bc) -> Parser bc t a -> Parser bc' t a
mapBreadcrumbs _ (Pure a)         = Pure a
mapBreadcrumbs t (LiftA2 f pa pb) = LiftA2 f (mapBreadcrumbs t pa) (mapBreadcrumbs t pb)
mapBreadcrumbs t GetCrumbs        = fmap t GetCrumbs
mapBreadcrumbs t (Target p pa)    = Target p $ mapBreadcrumbs t pa
mapBreadcrumbs t (OnChildren pa)  = OnChildren $ mapBreadcrumbs t pa
mapBreadcrumbs _ Current          = Current
mapBreadcrumbs t (Expect pa)      = Expect $ mapBreadcrumbs t pa
mapBreadcrumbs _ Fail             = Fail

