module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda

-- Input
data Event = Event
    { resource :: String }
    deriving (Generic, FromJSON)

-- Output
data Response = Response
    { statusCode :: Int
    , body :: String
    } deriving (Generic, ToJSON)

html = "<h1>Hello World</h1>"

handler :: Event -> Context -> IO (Either String Response)
handler event context =
    pure $ Right Response { statusCode = 200 , body = html }
