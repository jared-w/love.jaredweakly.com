module Lib where

import           GHC.Generics
import           Data.Aeson
import           Aws.Lambda
import           System.Random
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy                 ( Text )
import           Text.Mustache
import qualified Text.Mustache.Compile.TH      as TH
import           Generate

data Event = Event
    { resource :: Text }
    deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: Text
    } deriving (Show, Generic, ToJSON)

quotes :: [Text]
quotes =
  [ "You're my favorito, baberrito"
  , "Neato mosquito, baberrito"
  , "You're my favorite"
  , "Did you  know that you're my mostest favoritest in the whole wide world?"
  , "Snug me"
  , "I'm a big fan of the snugs"
  , "Sneep Snoop"
  , "I love you"
  , "ily"
  , "ilysm <3"
  , "You so cute"
  , "I love you the mostest"
  , "You make my heart feel melty"
  , "Hey I luff you"
  ]

-- | Themes currently must be a list of 5 colors that are defined as HSL(A)
-- triplets
-- This restriction will be lifted later when I become less lazy about CSS
themes :: [Theme]
themes = [greenTheme, toneTheme, starLight, bAndW]
 where
  mkTheme [l, pl, p, pd, d] = Theme l pl p pd d

  greenTheme :: Theme
  greenTheme = mkTheme
    [HSL 71 58 93, HSL 148 57 83, HSL 168 46 69, HSL 184 40 52, HSL 207 58 35]

  toneTheme :: Theme
  toneTheme = mkTheme
    [HSL 37 59 80, HSL 44 62 80, HSL 7 38 68, HSL 350 27 50, HSL 311 15 34]

  starLight :: Theme
  starLight = mkTheme
    [HSL 36 42 70, HSL 192 44 59, HSL 218 60 55, HSL 220 57 30, HSL 223 40 21]

  bAndW :: Theme
  bAndW = mkTheme [HSL 0 5 90, HSL 0 5 70, HSL 0 5 50, HSL 0 5 30, HSL 0 5 10]

getIn :: [a] -> IO a
getIn as = do
  n <- randomRIO (0, length as - 1)
  pure $ as !! n

handler :: Event -> Context -> IO (Either Text Response)
handler _ context = do
  q    <- getIn quotes
  t    <- getIn themes
  tmpl <- getIn [shadowTemplate, blurTemplate]

  pure $ Right Response
    { statusCode = 200
    , headers    = object ["Content-Type" .= ("text/html" :: Text)]
    , body       = tmpl $ ThemeData q t
    }
