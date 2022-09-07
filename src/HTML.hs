{-# LANGUAGE StrictData      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HTML where

import Control.Monad (join)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import Lasercutter
import Text.HTML.TagSoup (Tag(TagText, TagOpen))
import Text.HTML.TagSoup.Tree


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
  getChildren (TagBranch _ _ tts) = tts
  getChildren (TagLeaf _) = []

at :: Text -> Parser bc (TagTree Text) a -> Parser bc (TagTree Text) a
at t p
  = expect
  $ bool
      <$> pure Nothing
      <*> fmap Just p
      <*> onSelf (matchSelector $ HasTag t)

textOf :: TagTree a -> Maybe a
textOf = \case
  TagLeaf (TagText txt) -> Just txt
  _ -> Nothing

text :: Parser bc (TagTree a) (Maybe a)
text = onSelf textOf

getText :: Parser bc (TagTree Text) Text
getText = expect text

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

chroot :: Selector -> Parser bc HTML a -> Parser bc HTML a
chroot s = one . chroots s


chroots :: Selector -> Parser bc HTML a -> Parser bc HTML [a]
chroots = target . matchSelector


texts :: Parser bc HTML [Text]
texts = targetMap textOf


texts' :: Selector -> Parser bc HTML [Text]
texts' sel =
  fmap (catMaybes . join) $ target (matchSelector sel) $ onChildren text

isText :: HTML -> Bool
isText (TagLeaf (TagText _)) = True
isText _ = False


textNoScript :: Parser (Set Text) HTML [Text]
textNoScript =
  asum
    [ at "h1" texts
    , at "p" texts
    , at "b" texts
    ]


getTag :: HTML -> [Text]
getTag (TagBranch txt _ _) = pure txt
getTag (TagLeaf (TagOpen txt _)) = pure txt
getTag _ = mempty


main :: IO ()
main = traverse_ (traverse_ print) $ runParser getTag example
  $ fmap catMaybes
  $ target isText
  $ bool
      <$> text
      <*> pure Nothing
      <*> fmap ((`elem` ["script", "style"]) . head) breadcrumbs

