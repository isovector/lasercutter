{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Foldable
import           Control.Applicative (liftA2)
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Criterion.Main
import           Data.Foldable (find)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified HTML as Laser
import qualified Lasercutter as Laser
import qualified Text.HTML.Scalpel as Scalpel
import           Text.HTML.TagSoup (parseTags, Tag(..))
import           Text.HTML.TagSoup.Tree (tagTree, TagTree(..))
import GHC.Generics (Generic)


laserTitle :: Laser.Parser bc Laser.HTML Text
laserTitle = Laser.chroot (Laser.HasTag "title") $ Laser.one $ Laser.texts

laserContent :: Laser.Parser bc Laser.HTML [Text]
laserContent = Laser.chroot (Laser.Both (Laser.HasTag "div") (Laser.WithAttr "class" (== Just "blog-content row"))) $ Laser.texts



main :: IO ()
main = do
  !input_tags <- parseTags <$> T.readFile "bench/data/yes2.html"
  let !input_tree = fromJust $ find (Laser.matchSelector $ Laser.HasTag "html") $ tagTree input_tags

  let runLaser p = flip (Laser.runParser $ const ()) p input_tree

  print $ runLaser laserTitle
  print $ runLaser $ sequenceA $ replicate 10000 laserTitle
  print $ runLaser $ sequenceA $ replicate 100000 laserTitle
  print $ runLaser $ laserContent
  print $ runLaser $ liftA2 (,) laserTitle   laserContent

