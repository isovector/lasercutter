{-# LANGUAGE StrictData      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HTML where

import Lasercutter
import Data.Bool (bool)
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup (Tag(TagText, TagOpen))
import Text.HTML.TagSoup.Tree
import Data.Foldable (asum, traverse_)
import Control.Monad (join)

type HTML = TagTree Text

data Selector
  = Both Selector Selector
  | Alt Selector Selector
  | Negate Selector
  | HasTag Text
  | WithAttr Text (Maybe Text -> Bool)

matchSelector :: Selector -> TagTree Text -> Bool
matchSelector (Both se1 se2) tt                             = matchSelector se1 tt && matchSelector se2 tt
matchSelector (Alt se1 se2)  tt                             = matchSelector se1 tt || matchSelector se2 tt
matchSelector (Negate se)    tt                             = not $ matchSelector se tt
matchSelector (HasTag txt)  (TagBranch txt' _ _)            = txt == txt'
matchSelector (HasTag txt)  (TagLeaf (TagOpen txt' _))      = txt == txt'
matchSelector (HasTag _)    (TagLeaf _)                     = False
matchSelector (WithAttr txt f) (TagBranch _ x0 _)           = f $ lookup txt x0
matchSelector (WithAttr txt f)  (TagLeaf (TagOpen _ attrs)) = f $ lookup txt attrs
matchSelector (WithAttr _ _)   (TagLeaf _)                  = False

instance IsTree (TagTree t) where
  type Crumbs (TagTree t) = [t]

  getChildren (TagBranch _ _ tts) = tts
  getChildren (TagLeaf _) = []

  summarize (TagBranch t _ _) = [t]
  summarize (TagLeaf _) = []

at :: Text -> Parser (TagTree Text) a -> Parser (TagTree Text) a
at t p
  = Expect
  $ bool
      <$> pure Nothing
      <*> fmap Just p
      <*> Project (matchSelector $ HasTag t)

text :: Parser (TagTree a) (Maybe a)
text =
    Project (\case
          TagLeaf (TagText txt) -> Just txt
          _ -> Nothing)

getText :: Parser (TagTree Text) Text
getText = Expect text

example :: TagTree Text
example =
  TagBranch "html" [("lang", "en")]
    [ TagBranch "head" []
      [ TagBranch "title" [] [ TagLeaf $ TagText "Hello World!" ]
      , TagBranch "style" [("type", "text/css")]
          [ TagLeaf $ TagText "css"
          ]
      ]
    , TagBranch "body" []
      [ TagBranch "h1" [] [ TagLeaf $ TagText "Hi" ]
      , TagBranch "p" [("id", "lorem")] [ TagLeaf $ TagText "lorem ipsum" ]
      , TagBranch "p" []
          [ TagLeaf $ TagText "more p"
          , TagBranch "b" []
              [ TagLeaf $ TagText "bold"
              ]
          , TagLeaf $ TagText "done"
          ]
      , TagBranch "script" []
          [ TagLeaf $ TagText "dont want no scripts"
          ]
      ]
    ]

chroot :: Selector -> Parser HTML a -> Parser HTML a
chroot s = one . chroots s

chroots :: Selector -> Parser HTML a -> Parser HTML [a]
chroots = Target . matchSelector


one :: Parser HTML [a] -> Parser HTML a
one = Expect . fmap listToMaybe


texts :: Parser HTML [Text]
texts = Target isText getText


texts' :: Selector -> Parser HTML [Text]
texts' sel =
  fmap (catMaybes . join) $ Target (matchSelector sel) $ OnChildren text

isText :: HTML -> Bool
isText (TagLeaf (TagText _)) = True
isText _ = False


textNoScript :: Parser HTML [Text]
textNoScript =
  asum
    [ at "h1" texts
    , at "p" texts
    , at "b" texts
    ]


main :: IO ()
main = traverse_ (traverse_ print) $ runParser example
  $ fmap catMaybes
  $ Target isText
  $ bool
      <$> text
      <*> pure Nothing
      <*> fmap ((`elem` ["script", "style"]) . head) GetCrumbs

