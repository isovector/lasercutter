{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Criterion.Main
import           Data.Foldable (find)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import qualified HTML as Laser
import qualified Lasercutter as Laser
import qualified Text.HTML.Scalpel as Scalpel
import           Text.HTML.TagSoup (parseTags, Tag(..))
import           Text.HTML.TagSoup.Tree (tagTree, TagTree(..))

deriving stock instance Generic (TagTree Text)
deriving stock instance Generic (Tag Text)
deriving anyclass instance NFData (TagTree Text)
deriving anyclass instance NFData (Tag Text)


scalpelTitle :: Scalpel.Scraper Text Text
scalpelTitle = Scalpel.chroot "title" $ Scalpel.text "title"

laserTitle :: Laser.Parser Laser.HTML Text
laserTitle = Laser.chroot (Laser.HasTag "title") $ Laser.one $ Laser.texts

-- TODO(sandy): there is a faster version here using `texts` directly
scalpelContent :: Scalpel.Scraper Text [Text]
scalpelContent =
  let sel = "div" Scalpel.@: [ "class" Scalpel.@= "blog-content row" ]
   in Scalpel.chroot sel $ Scalpel.texts sel

laserContent :: Laser.Parser Laser.HTML [Text]
laserContent = Laser.chroot (Laser.Both (Laser.HasTag "div") (Laser.WithAttr "class" (== Just "blog-content row"))) $ Laser.texts



main :: IO ()
main = do
  !input_tags <- force . parseTags <$> T.readFile "bench/data/yes2.html"
  !input_tree <- evaluate $ force $ fromJust $ find (Laser.matchSelector $ Laser.HasTag "html") $ tagTree input_tags

  let
      benchScalpel p = bench "scalpel"     $ whnf (     Scalpel.scrape  p) input_tags
      benchLaser p   = bench "lasercutter" $ whnf (flip Laser.runParser p) input_tree
  -- print $ (Scalpel.scrape scalpelContent) input_tags
  -- print $ (flip Laser.runParser laserContent) input_tree
  defaultMain
    [ bgroup "title"
        [ benchScalpel scalpelTitle
        , benchLaser   laserTitle
        ]
    , bgroup "title x100"
        [ benchScalpel $ sequenceA $ replicate 100 scalpelTitle
        , benchLaser   $ sequenceA $ replicate 100 laserTitle
        ]
    , bgroup "title x1000"
        [ benchScalpel $ sequenceA $ replicate 1000 scalpelTitle
        , benchLaser   $ sequenceA $ replicate 1000 laserTitle
        ]
    , bgroup "content"
        [ benchScalpel $ scalpelContent
        , benchLaser   $ laserContent
        ]
    , bgroup "content and title"
        [ benchScalpel $ liftA2 (,) scalpelTitle scalpelContent
        , benchLaser   $ liftA2 (,) laserTitle   laserContent
        ]
    ]

