module Lasercutter where

import Text.HTML.TagSoup.Tree
import Data.Text (Text)
import GHC.Base (liftA2, join)
import Control.Applicative (liftA)
import Data.Maybe (isJust, mapMaybe, fromMaybe, listToMaybe, catMaybes)
import Data.Void (absurd)
import Text.HTML.TagSoup (Tag(TagText))
import Data.Bool (bool)
import Debug.Trace (trace)
import Control.Applicative
import Data.Foldable (asum)


data Selector
  = Both Selector Selector
  | Alt Selector Selector
  | Negate Selector
  | HasTag Text
  | HasAttr Text
  deriving (Eq, Ord, Show, Read)


data Parser a where
  Pure       :: a -> Parser a
  LiftA2     :: (b -> c -> a) -> Parser b -> Parser c -> Parser a
  Match      :: Selector -> Parser Bool
  Find       :: Selector -> Parser a -> Parser (Maybe a)
  OnChildren :: Parser a -> Parser [a]
  Try        :: Parser a -> Parser (Maybe a)
  Project    :: (TagTree Text -> a) -> Parser a
  FromMaybe  :: Parser a -> Parser (Maybe a) -> Parser a
  Fail       :: Parser a

instance Functor Parser where
  fmap = liftA

instance Applicative Parser where
  pure = Pure
  liftA2 _ Fail _ = Fail
  liftA2 _ _ Fail = Fail
  liftA2 f a b    = LiftA2 f a b

instance Alternative Parser where
  empty = Fail
  pa1 <|> pa2 = FromMaybe pa2 $ try pa1


matchSelector :: Selector -> TagTree Text -> Bool
matchSelector (Both se1 se2) tt                  = matchSelector se1 tt && matchSelector se2 tt
matchSelector (Alt se1 se2)  tt                  = matchSelector se1 tt || matchSelector se2 tt
matchSelector (Negate se)    tt                  = not $ matchSelector se tt
matchSelector (HasTag txt)  (TagBranch txt' _ _) = txt == txt'
matchSelector (HasTag _)    (TagLeaf _)          = False
matchSelector (HasAttr txt) (TagBranch _ x0 _)   = isJust $ lookup txt x0
matchSelector (HasAttr _)   (TagLeaf _)          = False


data Bind a where
  Bind :: Parser a -> ([a] -> Parser b) -> Bind b


split :: Parser a -> TagTree Text -> Bind a
split (Pure a) _         = Bind (pure ()) $ const $ pure a
split (LiftA2 f l r) tt  =
  case (split l tt, split r tt) of
    (Bind l' kl, Bind r' kr) ->
      Bind (liftA2 (,) l' r') $ \(unzip -> (lcs, rcs)) ->
        liftA2 f (kl lcs) (kr rcs)
split (Match se) tt       = Bind (pure ()) $ const $ pure $ matchSelector se tt
split p0@(Find se pa) tt
  | matchSelector se tt
  = bind (fmap Just) $ split pa tt
  | otherwise
  = Bind p0 $ pure . listToMaybe . catMaybes
split (OnChildren pa') _  = Bind pa' pure
split (Try pa) tt         = bind try $ split pa tt
split (Project f) tt      = Bind (pure ()) $ const $ pure $ f tt
split (FromMaybe a ma) tt = bind (FromMaybe a) $ split ma tt
split Fail _              = Bind Fail $ const $ Fail

try :: Parser a -> Parser (Maybe a)
try Fail = pure Nothing
try p    = fmap Just p

bind :: (Parser a -> Parser b) -> Bind a -> Bind b
bind f (Bind p k) = Bind p $ f . k


parseHTML :: Parser a -> TagTree Text -> Parser a
parseHTML p tt =
  case split p tt of
    Bind (Pure a) k -> k [a]
    Bind pa k -> parseHTMLs pa (getChildren tt) k


parseHTMLs :: Parser a -> [TagTree Text] -> ([a] -> Parser b) -> Parser b
parseHTMLs pa tts k = k $ mapMaybe (getResult . parseHTML pa) tts
-- parseHTMLs pa [] k = maybe Fail (k . pure) $ getResult pa
-- parseHTMLs pa (tt : tts') k =
--   case parseHTML pa tt of
--     Fail -> parseHTMLs pa tts' k
--     pa' -> parseHTMLs pa' tts' k


getResult :: Parser a -> Maybe a
getResult (Pure a) =  pure a
getResult (LiftA2 f a b) =  liftA2 f (getResult a) (getResult b)
getResult (Match _) = Nothing
getResult (Find _ _) = Nothing
getResult (OnChildren p) =
  case getResult p of
    Just a -> pure [a]
    _ -> Nothing
getResult (Project _) = Nothing
getResult (Try p) = Just $ getResult p
getResult (FromMaybe a ma) = case getResult ma of
  Nothing -> getResult a
  Just m_a -> m_a
getResult Fail = Nothing

getChildren :: TagTree Text -> [TagTree Text]
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


expect :: Parser (Maybe a) -> Parser a
expect = FromMaybe Fail



at :: Text -> Parser a -> Parser [a]
at t p
  = FromMaybe Fail
  $ bool
      <$> pure Nothing
      <*> fmap Just (OnChildren p)
      <*> Match (HasTag t)

singular :: Parser [a] -> Parser a
singular = FromMaybe Fail . fmap listToMaybe

getText :: Parser Text
getText =
  FromMaybe Fail $
    Project (\case
          TagLeaf (TagText txt) -> Just txt
          _ -> Nothing)

runParser :: TagTree Text -> Parser a -> Maybe a
runParser tt = getResult . flip parseHTML tt

asIs :: Parser (TagTree Text)
asIs = Project id

main :: IO ()
main = print $ runParser example $
    (Find (HasTag "h2") $ singular $ OnChildren getText) <|>
    (Find (HasTag "h1") $ singular $ OnChildren getText)



