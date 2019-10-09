{-# Language QuasiQuotes #-}
module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda
import Text.RawString.QQ
import System.Random

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
  , "I love you"
  , "ily"
  , "ilysm <3"
  , "You so cute"
  , "I love you the mostest"
  , "You make my heart feel melty"
  , "Hey I luff you"
  ]

html :: String -> String
html q = [r|<!DOCTYPE html>
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
        :root {
            --light: hsla(71, 58%, 93%, 1);
            --primary-light: hsla(148, 57%, 83%, 1);
            --primary: hsla(168, 46%, 69%, 1);
            --primary-dark: hsla(184, 40%, 52%, 1);
            --dark: hsla(207, 58%, 35%, 1);
        }

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

getInList :: IO Int
getInList = getStdRandom (randomR (0, length quotes - 1))

handler :: Event -> Context -> IO (Either String Response)
handler _ context = do
  n <- getInList
  let quote = quotes !! n
  pure $ 
    Right Response
        { statusCode = 200
        , headers = object [
            "Content-Type" .= ("text/html" :: String)
        ]
        , body = html quote
        }
