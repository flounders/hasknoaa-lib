module Hasknoaa where

import Control.Lens
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wreq as W
import Numeric (showFFloat)

data Units =
  USC -- ^ United States Customary units, such as Fahrenheit, miles per hour, etc.
  | SI -- ^ International System units and derivitives like Celsius, kilometers per hour, etc.
  deriving (Show)

safeGetWith ::
  W.Options 
  -> String
  -> IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
safeGetWith opts url = do
  let handler :: C.HttpException -> IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
      handler (C.HttpExceptionRequest _ e) =
        pure $ Left e
  (Right <$> W.getWith opts url) `E.catch` handler

-- | This function queries 'https://api.weather.gov/points/' with the latitude and longitude
-- provided by the function call.
getPointResponseFromApi ::
  Double -- ^ Latitude
  -> Double -- ^ Longitude
  -> IO (Either C.HttpExceptionContent (W.Response BSL.ByteString)) -- ^ Raw response from the API.
getPointResponseFromApi lat long = do
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"]
  let f x = showFFloat (Just 4) x ""
  safeGetWith reqOpts ("https://api.weather.gov/points/" <> f lat <> "," <> f long)

-- | This function queries a 'https://api.weather.gov/gridpoints/{wfo}/{x,y}/forecast' URL as
-- given by a response from a 'points' endpoint request.
getForecastResponseFromApi ::
  String -- ^ The forecast URL we are querying.
  -> Units -- ^ The units we want our forecast in.
  -> IO (Either C.HttpExceptionContent (W.Response BSL.ByteString)) -- ^ Raw response from the API.
getForecastResponseFromApi url units = do
  let u = case units of
            USC -> "us"
            SI -> "si"
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"] & W.param (T.pack "units") .~ [T.pack u]
  safeGetWith reqOpts url
