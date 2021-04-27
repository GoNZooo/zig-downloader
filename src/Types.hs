{-# LANGUAGE DeriveAnyClass #-}

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
import qualified Utilities

-- | Command line arguments
data Options = Options
  { optionsSettings :: !Settings,
    optionsCommand :: !Command,
    optionsQuiet :: !Bool
  }
  deriving (Eq, Show, Generic)

data Command
  = ShowCommand Text
  | ListCommand
  | DownloadCommand Text
  deriving (Eq, Show, Generic, FromJSON)

data Settings = Settings
  { settingsVerbose :: !Bool,
    settingsDownloadPath :: !Text,
    settingsDownloadMaster :: !Bool
  }
  deriving (Eq, Show, Generic)

parseJSONOptions :: String -> JSON.Options
parseJSONOptions prefix =
  JSON.defaultOptions {JSON.fieldLabelModifier = removePrefix prefix}

instance FromJSON Options where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "options"

instance FromJSON Settings where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "settings"

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
    -- @TODO: parse version names according to semantic versioning so they can be sorted properly
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
    versionArchitectures :: HashMap Text ArchiveSpecification
  }
  deriving (Eq, Show)

instance FromJSON Version where
  parseJSON value = do
    metadata <- JSON.parseJSON value
    architectures <- parseArchitectures value

    pure $ Version {versionMetadata = metadata, versionArchitectures = architectures}

parseArchitectures :: JSON.Value -> Parser (HashMap Text ArchiveSpecification)
parseArchitectures = JSON.withObject "Architectures" $ \o -> do
  let objectWithoutMetadata =
        Utilities.deleteAllKeys ["version", "date", "docs", "stdDocs", "src"] o
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
