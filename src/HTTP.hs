module HTTP where

import Network.HTTP.Client
import RIO
import Types

downloadBinaryFile :: Url -> FilePath -> RIO App ()
downloadBinaryFile (Url url) path = do
  manager <- asks appTlsManager
  request <- liftIO $ parseRequest url
  response <- liftIO $ httpLbs request manager
  writeFileBinary path $ toStrictBytes $ responseBody response
