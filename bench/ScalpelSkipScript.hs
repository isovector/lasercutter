module ScalpelSkipScript (textsWithoutScripts) where

import           Control.Applicative
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.Scalpel

textsWithoutScripts :: Selector -> Scraper Text [Text]
textsWithoutScripts sel = chroot sel $ textWithoutScripts

textWithoutScripts :: Scraper Text [Text]
textWithoutScripts = fmap (filter (not . T.null) . fmap T.strip) $ inSerial $ many $ stepNext innerScraper
  where
    innerScraper :: Scraper Text Text
    innerScraper = plainText
               <|> skip
               <|> fmap (T.intercalate " " . filter (not . T.null)) unknown

    plainText :: Scraper Text Text
    plainText  = fmap T.strip $ text (textSelector `atDepth` 0)

    skipMe :: Selector -> Scraper Text Text
    skipMe what = "" <$ recurseOn what

    skip :: Scraper Text Text
    skip     = asum
      [ skipMe "style"
      , skipMe "script"
      , skipMe "noscript"
      , skipMe "li"
      , skipMe "ul"
      , skipMe "ol"
      , skipMe "iframe"
      , skipMe "nav"
      , skipMe "object"
      , skipMe "source"
      , skipMe "svg"
      , skipMe "template"
      , skipMe "track"
      , skipMe "select"
      , skipMe "option"
      , skipMe "button"
      , skipMe "canvas"
      , skipMe "nav"
      , skipMe "h1"
      , skipMe "h2"
      , skipMe "h3"
      , skipMe "h4"
      , skipMe "h5"
      , skipMe "h6"
      , skipMe "sup"
      , skipMe "sub"
      ]

    unknown   = recurseOn anySelector

    recurseOn tag = chroot (tag `atDepth` 0) $ textWithoutScripts
