module Hasknoaa (someFunc) where

import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wreq as W
import Numeric (showFFloat)

getPointResponseFromApi ::
  Double
  -> Double
  -> IO (W.Response BSL.ByteString)
getPointResponseFromApi lat long = do
  let reqOpts = W.defaults & W.header H.hContentType .~ [(BSC.pack "application/ld+json")]
  let f x = showFFloat (Just 4) x ""
  W.getWith reqOpts ("https://api.weather.gov/points/" <> f lat <> "," <> f long)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
