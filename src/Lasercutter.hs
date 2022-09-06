{-# LANGUAGE FunctionalDependencies #-}

module Lasercutter where

import Control.Applicative
import Control.Monad (join)
import Data.Bool (bool)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Debug.RecoverRTTI
import Text.HTML.TagSoup.Tree


data Parser cr t a where
  Pure       :: a -> Parser cr t a
  GetCrumbs  :: Parser cr t cr
  LiftA2     :: (b -> c -> a) -> Parser cr t b -> Parser cr t c -> Parser cr t a
  Target     :: (t -> Bool) -> Parser cr t a -> Parser cr t [a]
  OnChildren :: Parser cr t a -> Parser cr t [a]
  Try        :: Parser cr t a -> Parser cr t (Maybe a)
  Project    :: (t -> a) -> Parser cr t a
  Expect     :: Parser cr t (Maybe a) -> Parser cr t a
  -- FromMaybe  :: Parser cr t a -> Parser cr t (Maybe a) -> Parser cr t a
  Fail       :: Parser cr t a

instance Show (Parser cr t a) where
  show = anythingToString

instance Functor (Parser cr t) where
  fmap = liftA

instance Applicative (Parser cr t) where
  pure = Pure
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b

instance Alternative (Parser cr t) where
  empty = Fail
  pa1 <|> pa2 = expect $ maybe <$> try pa2 <*> pure Just <*> try pa1



data Bind cr t a where
  Bind :: Parser cr t a -> ([a] -> Parser cr t b) -> Bind cr t b


split :: cr -> Parser cr t a -> t -> Bind cr t a
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

expect :: Parser cr t (Maybe a) -> Parser cr t a
expect (Try p) = p
expect Fail    = Fail
expect p       = Expect p

try :: Parser cr t a -> Parser cr t (Maybe a)
try Fail       = pure Nothing
try (Expect p) = p
try p          = fmap Just p

bind :: (Parser cr t a -> Parser cr t b) -> Bind cr t a -> Bind cr t b
bind f (Bind p k) = Bind p $ f . k


parseNode :: IsTree cr t => cr -> Parser cr t a -> t -> Parser cr t a
parseNode cr p tt =
  case split cr' p tt of
    Bind (Pure a) k -> k [a]
    Bind pa k -> parseChildren cr' pa (getChildren tt) k
  where
    cr' = summarize tt <> cr



parseChildren :: IsTree cr t => cr -> Parser cr t a -> [t] -> ([a] -> Parser cr t b) -> Parser cr t b
parseChildren cr pa tts k = k $ mapMaybe (getResult . parseNode cr pa) tts


getResult :: Parser cr t a -> Maybe a
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

class Monoid cr => IsTree cr t | t -> cr where
  getChildren :: t -> [t]
  summarize :: t -> cr


onSingleChild :: Parser cr t a -> Parser cr t (Maybe a)
onSingleChild = fmap listToMaybe . OnChildren


runParser :: IsTree cr t => t -> Parser cr t a -> Maybe a
runParser tt = getResult . flip (parseNode mempty) tt

asIs :: Parser cr t t
asIs = Project id

yo :: TagTree Text
yo = TagBranch "yo" [] []


failOnFalse :: a -> Parser cr t Bool -> Parser cr t a
failOnFalse a p = Expect $ bool <$> pure Nothing <*> pure (Just a) <*> p

