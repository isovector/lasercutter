
module Lasercutter where

import Debug.RecoverRTTI
import Control.Applicative
import Data.Bool (bool)
import Data.Maybe (isJust, mapMaybe, listToMaybe, catMaybes, fromMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup (Tag(TagText))
import Text.HTML.TagSoup.Tree
import Data.Either (partitionEithers)
import Control.Monad (join)
import Data.Foldable (asum)


data Selector
  = Both Selector Selector
  | AltS Selector Selector
  | Negate Selector
  | HasTag Text
  | HasAttr Text
  deriving (Eq, Ord, Show, Read)


data Parser t a where
  Pure       :: a -> Parser t a
  LiftA2     :: (b -> c -> a) -> Parser t b -> Parser t c -> Parser t a
  Find       :: (t -> Bool) -> Parser t a -> Parser t (Maybe a)
  OnChildren :: Parser t a -> Parser t [a]
  Try        :: Parser t a -> Parser t (Maybe a)
  Project    :: (t -> a) -> Parser t a
  Expect     :: Parser t (Maybe a) -> Parser t a
  -- FromMaybe  :: Parser t a -> Parser t (Maybe a) -> Parser t a
  Alt        :: Parser t b -> Parser t c -> Parser t (Either b c)
  Fail       :: Parser t a
  NoResult   :: Parser t (Maybe a)

instance Show (Parser t a) where
  show = anythingToString

instance Functor (Parser t) where
  fmap = liftA

instance Applicative (Parser t) where
  pure = Pure
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b

instance Alternative (Parser t) where
  empty = Fail
  pa1 <|> pa2 = fromMaybe <$> pa2 <*> Try pa1


matchSelector :: Selector -> TagTree Text -> Bool
matchSelector (Both se1 se2) tt                  = matchSelector se1 tt && matchSelector se2 tt
matchSelector (AltS se1 se2)  tt                  = matchSelector se1 tt || matchSelector se2 tt
matchSelector (Negate se)    tt                  = not $ matchSelector se tt
matchSelector (HasTag txt)  (TagBranch txt' _ _) = txt == txt'
matchSelector (HasTag _)    (TagLeaf _)          = False
matchSelector (HasAttr txt) (TagBranch _ x0 _)   = isJust $ lookup txt x0
matchSelector (HasAttr _)   (TagLeaf _)          = False


data Bind t a where
  Bind :: Parser t a -> ([a] -> Parser t b) -> Bind t b


split :: Parser t a -> t -> Bind t a
split (Pure a) _         = Bind (pure ()) $ const $ pure a
split (LiftA2 f l r) tt  =
  case (split l tt, split r tt) of
    (Bind l' kl, Bind r' kr) ->
      Bind (LiftA2 (,) l' r') $ \(unzip -> (lcs, rcs)) ->
        LiftA2 f (kl lcs) (kr rcs)
split p0@(Find se pa) tt
  | se tt
  = bind (fmap Just) $ split pa tt
  | otherwise
  = Bind p0 $ pure . listToMaybe . catMaybes
split (Expect pa) tt  = bind Expect $ split pa tt
split (OnChildren pa') _  = Bind pa' pure
split NoResult _          = Bind (pure ()) $ const $ NoResult
split (Try pa) tt         = split (try pa) tt
split (Project f) tt      = Bind (pure ()) $ const $ pure $ f tt
split (Alt a1 a2) tt =
  case (split a1 tt, split a2 tt) of
    (Bind a1' ka1, Bind a2' ka2) ->
      Bind (Alt a1' a2') $ \(partitionEithers -> (bs, cs)) ->
        Alt (ka1 bs) (ka2 cs)

-- split (FromMaybe a ma) tt =
--   case (split a tt, split ma tt) of
--     (Bind a' ka, Bind ma' kma) ->
--       Bind ma' $ FromMaybe (ka $ _ a') . kma


  -- case (split a tt, split ma tt) of
  --   (Bind a' ka, Bind ma' kma) ->
  --     Bind (liftA2 (,) a' ma') $ \(unzip -> (acs, macs)) ->
  --       FromMaybe (ka acs) (kma macs)
      -- bind (FromMaybe a) $ split ma tt
split Fail _              = Bind Fail $ const $ Fail

try :: Parser t a -> Parser t (Maybe a)
try Fail = pure Nothing
-- try (Try p) = Try (try p)
-- try (FromMaybe a p) = FromMaybe (try a) $ try p
-- try (LiftA2 f b c) = LiftA2 (liftA2 f) (try b) (try c)
-- try (OnChildren p) = fmap sequenceA $ OnChildren (try p)
try p    = fmap Just p

bind :: (Parser t a -> Parser t b) -> Bind t a -> Bind t b
bind f (Bind p k) = Bind p $ f . k


parseHTML :: IsTree t => Parser t a -> t -> Parser t a
parseHTML p tt =
  case split p tt of
    Bind (Pure a) k -> k [a]
    Bind pa k -> parseHTMLs pa (getChildren tt) k


parseHTMLs :: IsTree t => Parser t a -> [t] -> ([a] -> Parser t b) -> Parser t b
parseHTMLs pa tts k = k $ mapMaybe (getResult . parseHTML pa) tts
-- parseHTMLs pa [] k = maybe Fail (k . pure) $ getResult pa
-- parseHTMLs pa (tt : tts') k =
--   case parseHTML pa tt of
--     Fail -> parseHTMLs pa tts' k
--     pa' -> parseHTMLs pa' tts' k


getResult :: Parser t a -> Maybe a
getResult (Pure a) =  pure a
getResult (LiftA2 f a b) =  liftA2 f (getResult a) (getResult b)
getResult (Find _ _) = Just Nothing
getResult NoResult = Nothing
getResult (Expect pa) = join $ getResult pa
getResult (OnChildren p) =
  case getResult p of
    Just a -> pure [a]
    _ -> Nothing
getResult (Project _) = Nothing
getResult (Try p) = Just $ getResult p
getResult (Alt a1 a2) = maybe (fmap Right $ getResult a2) (Just . Left) $ getResult a1
-- getResult (FromMaybe a ma) = case getResult ma of
--   Nothing -> Nothing
--   Just Nothing -> getResult a
--   Just (Just z) -> Just z
getResult Fail = Nothing

class IsTree t where
  getChildren :: t -> [t]

instance IsTree (TagTree t) where
  getChildren (TagBranch _ _ tts) = tts
  getChildren (TagLeaf _) = []



example :: TagTree Text
example =
  TagBranch "html" [("lang", "en")]
    [ TagBranch "head" []
      [ TagBranch "title" [] [ TagLeaf $ TagText "Hello World!" ]
      ]
    , TagBranch "body" []
      [ TagBranch "h1" [] [ TagLeaf $ TagText "Hi" ]
      , TagBranch "p" [("id", "lorem")] [ TagLeaf $ TagText "lorem ipsum" ]
      ]
    ]


expect :: Parser t (Maybe a) -> Parser t a
expect = Expect

fromMaybe' :: Parser t a -> Parser t (Maybe a) -> Parser t a
fromMaybe' a2 a1 = fmap (either id id) $ Alt (Expect a1) a2



at :: Text -> Parser (TagTree Text) a -> Parser (TagTree Text) [a]
at t p
  = Expect
  $ bool
      <$> pure Nothing
      <*> fmap Just (OnChildren p)
      <*> Project (matchSelector $ HasTag t)

singular :: Parser t [a] -> Parser t a
singular = fromMaybe' Fail . fmap listToMaybe

getText :: Parser (TagTree Text) Text
getText =
  Expect $
    Project (\case
          TagLeaf (TagText txt) -> Just txt
          _ -> Nothing)

runParser :: IsTree t => t -> Parser t a -> Maybe a
runParser tt = getResult . flip parseHTML tt

asIs :: Parser t t
asIs = Project id

yo :: TagTree Text
yo = TagBranch "yo" [] []

main :: IO ()
main = print $ runParser example $

    at "html" asIs -- <|>
    -- at "bad" asIs

  -- getText <|> pure ""

