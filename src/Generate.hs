{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}

module Generate where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy                 ( Text )
import           Text.Mustache
import qualified Text.Mustache.Compile.TH      as TH

data ThemeData = ThemeData
    { title :: Text
    , theme :: Theme
    , background :: String
    } deriving (Show, Generic, ToJSON)

data HSL = HSL
    {
      -- | Defines a degree on the color wheel (from 0 to 360)
      -- 0 or 360 is red
      -- 120 is green
      -- 240 is blue
      hue :: Integer
      -- | Defines the saturation; 0% is a shade of gray and 100% is the full color (full saturation)
    , saturation :: Integer
      -- | Defines the lightness; 0% is black, 50% is normal, and 100% is white
    , lightness :: Integer
    } deriving (Eq, Show)

instance ToJSON HSL where
  toJSON hsl = toJSON $ hslToString hsl

hslToString :: HSL -> Text
hslToString (HSL h s l) = TL.concat $ TL.pack <$> [show h, ", " <> show s <> "%", ", " <> show l <> "%"]

data Theme = Theme
    { light :: HSL
    , primaryLight :: HSL
    , primary :: HSL
    , primaryDark :: HSL
    , dark :: HSL
    } deriving (Eq, Show)

instance ToJSON Theme where
  toJSON (Theme l pl p pd d) = toJSON $ themeToString [l, pl, p, pd, d]
    where
     themeToString :: [HSL] -> Text
     themeToString t = TL.concat $ zipWith (\a b -> a <> ": " <> hslToString b <> ";") vars t

     vars :: [Text]
     vars =
       ["--light", "--primary-light", "--primary", "--primary-dark", "--dark"]

shadowTemplate :: ThemeData -> Text
shadowTemplate d =
  let mainT = $(TH.compileMustacheFile "./src/templates/shadow.mustache")
      styleT =
          $(TH.compileMustacheFile "./src/templates/shadow.style.mustache")
  in  renderMustache (mainT <> styleT) (toJSON d)

blurTemplate :: ThemeData -> Text
blurTemplate d =
  let mainT = $(TH.compileMustacheFile "./src/templates/blur.mustache")
      styleT =
          $(TH.compileMustacheFile "./src/templates/blur.style.mustache")
  in  renderMustache (mainT <> styleT) (toJSON d)
