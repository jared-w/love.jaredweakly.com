{-# Language QuasiQuotes #-}
module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda
import Text.RawString.QQ

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

html :: String
html = [r|<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Document</title>
</head>
<body>
    <h1>Hello World</h1>
</body>
</html>
|]

handler :: Event -> Context -> IO (Either String Response)
handler event context = pure $ 
    Right Response
        { statusCode = 200
        , headers = object [
            "Content-Type" .= ("text/html" :: String)
        ]
        , body = html
        }
