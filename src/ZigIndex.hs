module ZigIndex where

import qualified Data.Aeson as JSON
import Network.HTTP.Client
import RIO
import Types

indexUrl :: String
indexUrl = "https://ziglang.org/download/index.json"

getVersions :: RIO App (Either String Versions)
getVersions = do
  manager <- asks appTlsManager
  request <- liftIO $ parseRequest indexUrl
  response <- liftIO $ httpLbs request manager

  pure $ JSON.eitherDecode' $ responseBody response

getMaster :: RIO App (Either String Master)
getMaster = fmap master <$> getVersions
