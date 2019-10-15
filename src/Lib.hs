{-# Language QuasiQuotes #-}
module Lib where

import           GHC.Generics
import           Data.Aeson
import           Aws.Lambda
import           Text.RawString.QQ
import           System.Random

-- Input
data Event = Event
    { resource :: String }
    deriving (Generic, FromJSON)

-- Output
data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: String
    } deriving (Generic, ToJSON)

quotes :: [String]
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

themes :: [[String]]
themes = [greenTheme, toneTheme, starLight, bAndW]
 where
  greenTheme :: [String]
  greenTheme =
    [ "hsl(71, 58%, 93%)"
    , "hsl(148, 57%, 83%)"
    , "hsl(168, 46%, 69%)"
    , "hsl(184, 40%, 52%)"
    , "hsl(207, 58%, 35%)"
    ]

  toneTheme :: [String]
  toneTheme = ["#EAD3AE", "#ECDBAD", "#CC948D", "#A25D68", "#654A60"]

  starLight :: [String]
  starLight = ["#DAB989", "#69B2C4", "#4578D1", "#213D77", "#202C4B"]

  bAndW :: [String]
  bAndW = ["#0f0f0f", "#3d3d3d", "#6b6b6b", "#a8a8a8", "#e6e6e6"]

mkTheme :: [String] -> String
mkTheme t = concat $ zipWith (\a b -> a <> ": " <> b <> ";") vars t
 where
  vars =
    ["--light", "--primary-light", "--primary", "--primary-dark", "--dark"]

html :: String -> [String] -> String
html q t =
  [r|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>|] <> q <> [r|</title>
    </head>
    <body>
        <h1>|] <> q <> [r|</h1>
    </body>
    <style>
        :root { |] <> mkTheme t <> [r| }

        *,
        *:before,
        *:after {
            box-sizing: border-box;
            padding: 0;
            margin: 0;
        }

        body {
            background-color: var(--light);
            display: grid;
            place-content: center;
        }

        html,
        body {
            height: 100%;
        }

        h1 {
            font-size: calc(2rem + 2.4vw * 1.5);
            color: var(--dark);
            padding: 10vmin;

            transition: text-shadow 0.25s ease-in;
        }

        h1:hover {
            transition: text-shadow 0.25s ease-out;

            /* prettier-ignore */
            text-shadow:
              0 1px 0px var(--primary-light),
              1px 0 0px var(--primary-dark),
              1px 2px 1px var(--primary-light),
              2px 1px 1px var(--primary-dark),
              2px 3px 2px var(--primary-light),
              3px 2px 2px var(--primary-dark),
              3px 4px 2px var(--primary-light),
              4px 3px 3px var(--primary-dark),
              4px 5px 3px var(--primary-light),
              5px 4px 2px var(--primary-dark),
              5px 6px 2px var(--primary-light),
              6px 5px 2px var(--primary-dark),
              6px 7px 1px var(--primary-light),
              7px 6px 1px var(--primary-dark),
              7px 8px 0px var(--primary-light),
              8px 7px 0px var(--primary-dark);
        }
    </style>
</html>
|]

getIn :: [a] -> IO a
getIn as = do
  n <- randomRIO (0, length as - 1)
  pure $ as !! n

handler :: Event -> Context -> IO (Either String Response)
handler _ context = do
  q <- getIn quotes
  t <- getIn themes
  pure $ Right Response
    { statusCode = 200
    , headers    = object ["Content-Type" .= ("text/html" :: String)]
    , body       = html q t
    }
