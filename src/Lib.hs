module Lib where

import           GHC.Generics
import           Data.Aeson
import           Aws.Lambda
import           System.Random
import           Data.Text.Lazy                 ( Text )
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

gradients :: [String]
gradients =
  [ "background-image: linear-gradient(0deg, hsl(var(--primary-dark)) 10%, hsl(var(--primary-light)) 90%);"
  , "background-image: linear-gradient(to right, hsl(var(--primary)), hsl(var(--primary-light)), hsl(var(--primary-dark)));"
  , "background-image: linear-gradient(90deg, hsl(var(--dark)) 0%, hsl(var(--primary-dark)) 35%, hsl(var(--primary)) 100%);"
  , "background-image: linear-gradient(122deg, hsl(var(--primary-light)) 0%, hsl(var(--primary-dark)) 100%);"
  ]

getIn :: [a] -> IO a
getIn as = do
  n <- randomRIO (0, length as - 1)
  pure $ as !! n

mkBody :: IO Text
mkBody = do
  q <- getIn quotes
  t <- getIn themes

  n <- randomRIO (0, 1 :: Int)
  let tmpl = [shadowTemplate, blurTemplate] !! n
  c <- [pure "background-color: hsl(var(--light));", getIn gradients] !! n
  pure . tmpl $ ThemeData q t c

handler :: Event -> Context -> IO (Either Text Response)
handler _ _ = do
  body <- mkBody
  pure $ Right Response
    { statusCode = 200
    , headers    = object ["Content-Type" .= ("text/html" :: Text)]
    , body       = body
    }
