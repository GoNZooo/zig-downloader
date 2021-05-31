module ZigIndex where

import HTTP
import RIO
import Types

indexUrl :: Url
indexUrl = Url "https://ziglang.org/download/index.json"

getVersions :: RIO App (Either String Versions)
getVersions = downloadJSON indexUrl

getMaster :: RIO App (Either String Master)
getMaster = fmap master <$> getVersions
