{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import RIO
import qualified RIO.HashMap as Map
import RIO.Process
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import Text.Inflections

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsDownloadPath :: !Text,
    optionsDownloadMaster :: !Bool
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
  { versionsMaster :: Version,
    versionsTags :: HashMap Text Version
  }
  deriving (Eq, Show, Generic)

instance FromJSON Versions where
  parseJSON = JSON.withObject "Versions" $ \o -> do
    master <- o .: "master"
    let initialMap = Map.empty
        mapWithoutMaster = Map.delete "master" o
        tags =
          Map.foldrWithKey
            ( \k v m -> case JSON.fromJSON v of
                JSON.Success archiveSpecification -> Map.insert k archiveSpecification m
                JSON.Error _e -> m
            )
            initialMap
            mapWithoutMaster

    pure $ Versions {versionsMaster = master, versionsTags = tags}

data Version = Version
  { versionMetadata :: VersionMetadata,
    versionVersions :: HashMap Text ArchiveSpecification
  }
  deriving (Eq, Show)

instance FromJSON Version where
  parseJSON value = do
    metadata <- JSON.parseJSON value
    versions <- parseVersions value

    pure $ Version {versionMetadata = metadata, versionVersions = versions}

parseVersions :: JSON.Value -> Parser (HashMap Text ArchiveSpecification)
parseVersions = JSON.withObject "Versions" $ \o -> do
  let objectWithoutMetadata = deleteAllKeys ["version", "date", "docs", "stdDocs", "src"] o
      versions =
        Map.foldrWithKey
          ( \k v m -> case JSON.fromJSON v of
              JSON.Success archiveSpecification -> Map.insert k archiveSpecification m
              JSON.Error _e -> m
          )
          Map.empty
          objectWithoutMetadata

  pure versions

data VersionMetadata = VersionMetadata
  { versionMetadataVersion :: Maybe Text,
    versionMetadataDate :: Text,
    versionMetadataDocs :: Text,
    versionMetadataStdDocs :: Maybe Text,
    versionMetadataSrc :: ArchiveSpecification
  }
  deriving (Eq, Show, Generic)

instance FromJSON VersionMetadata where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "versionMetadata"

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

deleteAllKeys :: [Text] -> HashMap Text a -> HashMap Text a
deleteAllKeys keys = Map.filterWithKey (\k _v -> k `notElem` keys)
