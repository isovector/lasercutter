module Lasercutter.Internal where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Lasercutter.Types


split :: bc -> Parser bc t a -> t -> Split bc t a
split _ (Pure a) _         = Split (pure ()) $ const $ pure a
split cr GetCrumbs _        = Split (pure ()) $ const $ pure cr
split cr (LiftA2 f l r) tt  =
  case (split cr l tt, split cr r tt) of
    (Split l' kl, Split r' kr) ->
      Split (LiftA2 (,) l' r') $ \(unzip -> (lcs, rcs)) ->
        LiftA2 f (kl lcs) (kr rcs)
split cr p0@(Target se pa) tt
  | se tt
  = bind (fmap pure) $ split cr pa tt
  | otherwise
  = Split p0 $ pure . join
split cr (Expect pa) tt  = bind expect $ split cr pa tt
split _ (OnChildren pa') _  = Split pa' pure
split _ (Project f) tt      = Split (pure ()) $ const $ pure $ f tt
split _ Fail _              = Split Fail $ const $ Fail


bind :: (Parser bc t a -> Parser bc t b) -> Split bc t a -> Split bc t b
bind f (Split p k) = Split p $ f . k


parseNode :: (Semigroup bc, IsTree t) => (t -> bc) -> bc -> Parser bc t a -> t -> Parser bc t a
parseNode summarize cr p tt =
  case split cr' p tt of
    Split (Pure a) k -> k [a]
    Split pa k -> parseChildren summarize cr' pa (getChildren tt) k
  where
    cr' = summarize tt <> cr


parseChildren :: (IsTree t, Semigroup bc) => (t -> bc) -> bc -> Parser bc t a -> [t] -> ([a] -> Parser bc t b) -> Parser bc t b
parseChildren summarize cr pa tts k = k $ mapMaybe (getResult . parseNode summarize cr pa) tts


getResult :: Parser bc t a -> Maybe a
getResult (Pure a) =  pure a
getResult GetCrumbs =  Nothing
getResult (LiftA2 f a b) =  liftA2 f (getResult a) (getResult b)
getResult (Target _ p) = case getResult p of
  Nothing -> Nothing
  Just a -> Just [a]
getResult (Expect pa) = join $ getResult pa
getResult (OnChildren p) =
  case getResult p of
    Just a -> pure [a]
    _ -> Nothing
getResult (Project _) = Nothing
getResult Fail = Nothing


runParser :: (Monoid bc, IsTree t) => (t -> bc) -> t -> Parser bc t a -> Maybe a
runParser summarize tt = getResult . flip (parseNode summarize mempty) tt

