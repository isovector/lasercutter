{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Bool (bool)
import qualified Data.Set as S
import Data.Set (Set)
import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Criterion.Main
import           Data.Foldable (find)
import           Data.Maybe (fromJust, catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import qualified HTML as Laser
import qualified Lasercutter as Laser
import qualified Text.HTML.Scalpel as Scalpel
import           Text.HTML.TagSoup (parseTags, Tag(..))
import           Text.HTML.TagSoup.Tree (tagTree, TagTree(..))
import qualified ScalpelSkipScript as Scalpel

deriving stock instance Generic (TagTree Text)
deriving stock instance Generic (Tag Text)
deriving anyclass instance NFData (TagTree Text)
deriving anyclass instance NFData (Tag Text)


scalpelTitle :: Scalpel.Scraper Text Text
scalpelTitle = Scalpel.chroot "title" $ Scalpel.text "title"

laserTitle :: Laser.Parser bc Laser.HTML Text
laserTitle = Laser.chroot (Laser.HasTag "title") $ Laser.one $ Laser.texts

-- TODO(sandy): there is a faster version here using `texts` directly
scalpelContent :: Scalpel.Scraper Text [Text]
scalpelContent =
  let sel = "div" Scalpel.@: [ "class" Scalpel.@= "blog-content row" ]
   in Scalpel.chroot sel $ Scalpel.texts sel


scalpelBodyText :: Scalpel.Scraper Text [Text]
scalpelBodyText = Scalpel.textsWithoutScripts "body"

laserContent :: Laser.Parser bc Laser.HTML [Text]
laserContent = Laser.chroot (Laser.Both (Laser.HasTag "div") (Laser.WithAttr "class" (== Just "blog-content row"))) $ Laser.texts

laserBodyText :: Laser.Parser (Set Text) Laser.HTML [Text]
laserBodyText = fmap (filter (not . T.null) . fmap T.strip) $ Laser.chroot (Laser.HasTag "body") $ fmap catMaybes $ Laser.Target Laser.isText $
  bool
    <$> Laser.text
    <*> pure Nothing
    <*> fmap (\bc -> any (flip S.member bc)
          [ "style"
          , "script"
          , "noscript"
          , "li"
          , "ul"
          , "ol"
          , "iframe"
          , "nav"
          , "object"
          , "source"
          , "svg"
          , "template"
          , "track"
          , "select"
          , "option"
          , "button"
          , "canvas"
          , "nav"
          , "h1"
          , "h2"
          , "h3"
          , "h4"
          , "h5"
          , "h6"
          , "sup"
          , "sub"
          ]) Laser.GetCrumbs


main :: IO ()
main = do
  !input_tags <- force . parseTags <$> T.readFile "bench/data/yes2.html"
  !input_tree <- evaluate $ force $ fromJust $ find (Laser.matchSelector $ Laser.HasTag "html") $ tagTree input_tags

  -- proof that we get equivalent results for bodytext
  print $ fmap (T.intercalate " ") (Scalpel.scrape scalpelBodyText input_tags)
       == fmap (T.intercalate " ") (Laser.runParser (S.fromList . Laser.getTag) input_tree laserBodyText)

  let
      benchScalpel p = bench "scalpel"     $ whnf (     Scalpel.scrape  p) input_tags
      benchLaser p   = bench "lasercutter" $ whnf (flip (Laser.runParser $ S.fromList . Laser.getTag) p) input_tree
      benchThem name ps pl =
        bgroup name
          [ benchScalpel ps
          , benchLaser pl
          ]

  defaultMain
    [ bgroup "title"
        [ benchScalpel scalpelTitle
        , benchLaser   laserTitle
        ]
    , bgroup "content"
        [ benchScalpel $ scalpelContent
        , benchLaser   $ laserContent
        ]
    , bgroup "content and title"
        [ benchScalpel $ liftA2 (,) scalpelTitle scalpelContent
        , benchLaser   $ liftA2 (,) laserTitle   laserContent
        ]
    , bgroup "body text"
        [ benchScalpel scalpelBodyText
        , benchLaser   laserBodyText
        ]
    ]

