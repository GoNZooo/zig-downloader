module ZigIndex where

import qualified Data.Aeson as JSON
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import RIO
import Types

indexUrl :: String
indexUrl = "https://ziglang.org/download/index.json"

getVersions :: IO (Either String Versions)
getVersions = do
  manager <- newTlsManager
  request <- parseRequest indexUrl
  response <- httpLbs request manager
  let body = responseBody response

  pure $ JSON.eitherDecode' body

getMaster :: IO (Either String Version)
getMaster = fmap versionsMaster <$> getVersions
