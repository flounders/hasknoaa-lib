{-# LANGUAGE OverloadedStrings #-}

module Hasknoaa where

import qualified Control.Exception as E
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Header as H
import qualified Network.Wreq as W
import Numeric (showFFloat)

data Point = Point
  { pointId :: String, -- Ok
    pointCounty :: String, -- Ok
    pointCwa :: String, -- Ok
    pointFireWeatherZone :: String, -- Ok
    pointForecast :: String, -- Ok
    pointForecastGridData :: String, -- Ok
    pointForecastHourly :: String, -- Ok
    pointForecastOffice :: String, -- Ok
    pointForecastZone :: String, -- Ok
    pointGridId :: String, -- Ok
    pointGridX :: Int,
    pointGridY :: Int,
    pointRadarStation :: String, -- Ok
    pointTimeZone :: String -- Ok
  }
  deriving (Show)

data Units
  = -- | United States Customary units, such as Fahrenheit, miles per hour, etc.
    USC
  | -- | International System units and derivitives like Celsius, kilometers per hour, etc.
    SI
  deriving (Show)

instance A.FromJSON Point where
  parseJSON (A.Object v) = do
    id <- v A..: "id"
    props <- v A..: "properties"
    county <- props A..: "county"
    cwa <- props A..: "cwa"
    fireWeatherZone <- props A..: "fireWeatherZone"
    forecast <- props A..: "forecast"
    forecastGridData <- props A..: "forecastGridData"
    forecastHourly <- props A..: "forecastHourly"
    forecastOffice <- props A..: "forecastOffice"
    forecastZone <- props A..: "forecastZone"
    gridId <- props A..: "gridId"
    gridX <- props A..: "gridX"
    gridY <- props A..: "gridY"
    radarStation <- props A..: "radarStation"
    timeZone <- props A..: "timeZone"
    pure $
      Point
        id
        county
        cwa
        fireWeatherZone
        forecast
        forecastGridData
        forecastHourly
        forecastOffice
        forecastZone
        gridId
        gridX
        gridY
        radarStation
        timeZone

decodePoint :: BSL.ByteString -> Maybe Point
decodePoint = A.decode

safeGetWith ::
  W.Options ->
  String ->
  IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
safeGetWith opts url = do
  let handler :: C.HttpException -> IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
      handler (C.HttpExceptionRequest _ e) =
        pure $ Left e
  (Right <$> W.getWith opts url) `E.catch` handler

-- | This function queries 'https://api.weather.gov/points/' with the latitude and longitude
-- provided by the function call.
getPointResponseFromApi ::
  -- | Latitude
  Double ->
  -- | Longitude
  Double ->
  -- | Raw response from the API.
  IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
getPointResponseFromApi lat long = do
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"]
  let f x = showFFloat (Just 4) x ""
  safeGetWith reqOpts ("https://api.weather.gov/points/" <> f lat <> "," <> f long)

-- | This function queries a 'https://api.weather.gov/gridpoints/{wfo}/{x,y}/forecast' URL as
-- given by a response from a 'points' endpoint request.
getForecastResponseFromApi ::
  -- | The forecast URL we are querying.
  String ->
  -- | The units we want our forecast in.
  Units ->
  -- | Raw response from the API.
  IO (Either C.HttpExceptionContent (W.Response BSL.ByteString))
getForecastResponseFromApi url units = do
  let u = case units of
        USC -> "us"
        SI -> "si"
  let reqOpts = W.defaults & W.header H.hContentType .~ [BSC.pack "application/ld+json"] & W.param (T.pack "units") .~ [T.pack u]
  safeGetWith reqOpts url
