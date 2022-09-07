module Lasercutter.Internal where

import Control.Applicative
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import Lasercutter.Types


split :: bc -> Parser bc t a -> t -> Bind bc t a
split _ (Pure a) _         = Bind (pure ()) $ const $ pure a
split cr GetCrumbs _        = Bind (pure ()) $ const $ pure cr
split cr (LiftA2 f l r) tt  =
  case (split cr l tt, split cr r tt) of
    (Bind l' kl, Bind r' kr) ->
      Bind (LiftA2 (,) l' r') $ \(unzip -> (lcs, rcs)) ->
        LiftA2 f (kl lcs) (kr rcs)
split cr p0@(Target se pa) tt
  | se tt
  = bind (fmap pure) $ split cr pa tt
  | otherwise
  = Bind p0 $ pure . join
split cr (Expect pa) tt  = bind expect $ split cr pa tt
split _ (OnChildren pa') _  = Bind pa' pure
split cr (Try pa) tt         = split cr (tryP pa) tt
split _ (Project f) tt      = Bind (pure ()) $ const $ pure $ f tt
split _ Fail _              = Bind Fail $ const $ Fail


bind :: (Parser bc t a -> Parser bc t b) -> Bind bc t a -> Bind bc t b
bind f (Bind p k) = Bind p $ f . k


parseNode :: (Semigroup bc, IsTree t) => (t -> bc) -> bc -> Parser bc t a -> t -> Parser bc t a
parseNode summarize cr p tt =
  case split cr' p tt of
    Bind (Pure a) k -> k [a]
    Bind pa k -> parseChildren summarize cr' pa (getChildren tt) k
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
getResult (Try p) = Just $ getResult p
getResult Fail = Nothing


runParser :: (Monoid bc, IsTree t) => (t -> bc) -> t -> Parser bc t a -> Maybe a
runParser summarize tt = getResult . flip (parseNode summarize mempty) tt
