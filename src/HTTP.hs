module HTTP where

import Data.Aeson (FromJSON, eitherDecode')
import Network.HTTP.Client
import RIO
import qualified RIO.ByteString.Lazy as LazyByteString
import Types

downloadJSON :: (FromJSON a) => Url -> RIO App (Either String a)
downloadJSON url = do
  data' <- downloadData url
  data' & eitherDecode' & pure

downloadBinaryFile :: Url -> FilePath -> RIO App ()
downloadBinaryFile url path = do
  data' <- downloadData url
  writeFileBinary path $ toStrictBytes data'

downloadData :: Url -> RIO App LazyByteString.ByteString
downloadData (Url url) = do
  manager <- view appTlsManager
  request <- liftIO $ parseRequest url
  response <- liftIO $ httpLbs request manager
  pure $ responseBody response
