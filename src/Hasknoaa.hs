module Hasknoaa (someFunc) where

import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wreq as W
import Numeric (showFFloat)

data Units =
  USC
  | SI
  deriving (Show)

getPointResponseFromApi ::
  Double
  -> Double
  -> IO (W.Response BSL.ByteString)
getPointResponseFromApi lat long = do
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"]
  let f x = showFFloat (Just 4) x ""
  W.getWith reqOpts ("https://api.weather.gov/points/" <> f lat <> "," <> f long)

getForecastResponseFromApi ::
  String
  -> Units
  -> IO (W.Response BSL.ByteString)
getForecastResponseFromApi url units = do
  let u = case units of
            USC -> "us"
            SI -> "si"
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"] & W.param (T.pack "units") .~ [T.pack u]
  W.getWith reqOpts url

someFunc :: IO ()
someFunc = putStrLn "someFunc"
