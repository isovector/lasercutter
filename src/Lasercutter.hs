{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData             #-}

module Lasercutter where

import Control.Applicative
import Control.Monad (join)
import Data.Bool (bool)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Debug.RecoverRTTI
import Text.HTML.TagSoup.Tree


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

instance Show (Parser bc t a) where
  show = anythingToString

instance Functor (Parser bc t) where
  fmap = liftA

instance Applicative (Parser bc t) where
  pure = Pure
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b

instance Alternative (Parser bc t) where
  empty = Fail
  pa1 <|> pa2 = expect $ maybe <$> try pa2 <*> pure Just <*> try pa1



data Bind bc t a where
  Bind :: Parser bc t a -> ([a] -> Parser bc t b) -> Bind bc t b


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
split cr (Try pa) tt         = split cr (try pa) tt
split _ (Project f) tt      = Bind (pure ()) $ const $ pure $ f tt
split _ Fail _              = Bind Fail $ const $ Fail

expect :: Parser bc t (Maybe a) -> Parser bc t a
expect (Try p) = p
expect Fail    = Fail
expect p       = Expect p

try :: Parser bc t a -> Parser bc t (Maybe a)
try Fail       = pure Nothing
try (Expect p) = p
try p          = fmap Just p

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

class IsTree t where
  getChildren :: t -> [t]


onSingleChild :: Parser bc t a -> Parser bc t (Maybe a)
onSingleChild = fmap listToMaybe . OnChildren


runParser :: (Monoid bc, IsTree t) => (t -> bc) -> t -> Parser bc t a -> Maybe a
runParser summarize tt = getResult . flip (parseNode summarize mempty) tt

asIs :: Parser bc t t
asIs = Project id

yo :: TagTree Text
yo = TagBranch "yo" [] []


failOnFalse :: a -> Parser bc t Bool -> Parser bc t a
failOnFalse a p = Expect $ bool <$> pure Nothing <*> pure (Just a) <*> p

