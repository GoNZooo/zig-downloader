{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZigIndex where

import qualified Data.Aeson as JSON
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Types

indexUrl :: String
indexUrl = "https://ziglang.org/download/index.json"

getMaster :: IO (Either String Master)
getMaster = do
  manager <- newTlsManager
  request <- parseRequest indexUrl
  response <- httpLbs request manager
  let body = responseBody response

  pure $ versionsMaster <$> JSON.eitherDecode' body
