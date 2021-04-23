{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import RIO
import RIO.Process
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import Text.Inflections

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsDownloadPath :: !Text
  }
  deriving (Show, Generic)

parseJSONOptions :: String -> JSON.Options
parseJSONOptions prefix =
  JSON.defaultOptions {JSON.fieldLabelModifier = removePrefix prefix}

instance FromJSON Options where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "options"

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

data ArchiveSpecification = ArchiveSpecification
  { archiveSpecificationTarball :: Text,
    archiveSpecificationShasum :: Text,
    archiveSpecificationSize :: Int
  }
  deriving (Eq, Show)

instance FromJSON ArchiveSpecification where
  parseJSON = JSON.withObject "ArchiveSpecification" $ \o -> do
    tarball <- o .: "tarball"
    shasum <- o .: "shasum"
    maybeSize <- readMaybe <$> o .: "size"
    case maybeSize of
      Just size ->
        pure $
          ArchiveSpecification
            { archiveSpecificationTarball = tarball,
              archiveSpecificationShasum = shasum,
              archiveSpecificationSize = size
            }
      Nothing ->
        fail "Size is not readable as integer"

data Versions = Versions
  { versionsMaster :: Master
  }
  deriving (Eq, Show, Generic)

instance FromJSON Versions where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "versions"

data Master = Master
  { masterVersion :: Text,
    masterDate :: Text,
    masterDocs :: Text,
    masterStdDocs :: Text,
    masterSrc :: ArchiveSpecification
  }
  deriving (Eq, Show, Generic)

instance FromJSON Master where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "master"

removePrefix :: String -> String -> String
removePrefix prefix string =
  let splits = PartialText.splitOn (Text.pack prefix) (Text.pack string)
   in case splits of
        [_prefix, rest] ->
          case camelizeCustom False <$> parseCamelCase [] rest of
            Right result -> Text.unpack result
            Left err -> throwM err
        [rest] -> Text.unpack rest
        _otherwise -> error "Invalid prefix used"
