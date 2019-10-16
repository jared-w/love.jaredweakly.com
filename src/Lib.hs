{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
module Lib where

import           GHC.Generics
import           Data.Aeson
import           Aws.Lambda
import           System.Random
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy                 ( Text )
import           Text.Mustache
import qualified Text.Mustache.Compile.TH      as TH

data Event = Event
    { resource :: Text }
    deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: Text
    } deriving (Show, Generic, ToJSON)

data ShadowData = ShadowData
    { title :: Text
    , theme :: Text
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

themes :: [[Text]]
themes = [greenTheme, toneTheme, starLight, bAndW]
 where
  greenTheme :: [Text]
  greenTheme =
    [ "hsl(71, 58%, 93%)"
    , "hsl(148, 57%, 83%)"
    , "hsl(168, 46%, 69%)"
    , "hsl(184, 40%, 52%)"
    , "hsl(207, 58%, 35%)"
    ]

  toneTheme :: [Text]
  toneTheme = ["#EAD3AE", "#ECDBAD", "#CC948D", "#A25D68", "#654A60"]

  starLight :: [Text]
  starLight = ["#DAB989", "#69B2C4", "#4578D1", "#213D77", "#202C4B"]

  bAndW :: [Text]
  bAndW = ["#0f0f0f", "#3d3d3d", "#6b6b6b", "#a8a8a8", "#e6e6e6"]

mkTheme :: [Text] -> Text
mkTheme t = TL.concat $ zipWith (\a b -> a <> ": " <> b <> ";") vars t
 where
  vars =
    ["--light", "--primary-light", "--primary", "--primary-dark", "--dark"]

shadowTemplate :: ShadowData -> Text
shadowTemplate d =
  let mainT = $(TH.compileMustacheFile "./src/templates/shadow.mustache")
      styleT =
          $(TH.compileMustacheFile "./src/templates/shadow.style.mustache")
  in  renderMustache (mainT <> styleT) (toJSON d)

getIn :: [a] -> IO a
getIn as = do
  n <- randomRIO (0, length as - 1)
  pure $ as !! n

handler :: Event -> Context -> IO (Either Text Response)
handler _ context = do
  q <- getIn quotes
  t <- getIn themes
  let shadowData = ShadowData q (mkTheme t)

  pure $ Right Response
    { statusCode = 200
    , headers    = object ["Content-Type" .= ("text/html" :: Text)]
    , body       = shadowTemplate shadowData
    }
