{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Time.Calendar (Day)
import Network.HTTP.Client
import Qtility
import qualified RIO.HashMap as Map
import RIO.Process
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import Text.Inflections
import qualified Utilities

newtype Url = Url {unUrl :: String}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

newtype ArchitectureName = ArchitectureName {unArchitectureName :: Text}
  deriving stock (Eq, Show, Generic)

newtype YamlFileDecodingError = YamlFileDecodingError {unYamlFileDecodingError :: String}
  deriving (Eq, Show)

instance Exception YamlFileDecodingError

-- | Command line arguments
data Options = Options
  { _optionsSettings :: !Settings,
    _optionsCommand :: !Command,
    _optionsQuiet :: !Bool
  }
  deriving (Eq, Show, Generic)

data Command
  = ShowCommand Text
  | ListCommand
  | DownloadCommand Text [ArchitectureName]
  deriving (Eq, Show, Generic)

data Settings = Settings
  { _settingsVerbose :: !Bool,
    _settingsDownloadPath :: !FilePath,
    _settingsDownloadMaster :: !Bool
  }
  deriving (Eq, Show, Generic)

data App = App
  { _appLogFunc :: !LogFunc,
    _appProcessContext :: !ProcessContext,
    _appOptions :: !Options,
    _appTlsManager :: !Manager
  }

data ArchiveSpecification = ArchiveSpecification
  { _archiveSpecificationTarball :: !Url,
    _archiveSpecificationShasum :: !Text,
    _archiveSpecificationSize :: !Int
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
            { _archiveSpecificationTarball = tarball,
              _archiveSpecificationShasum = shasum,
              _archiveSpecificationSize = size
            }
      Nothing ->
        fail "Size is not readable as integer"

data Versions = Versions
  { master :: !Master,
    -- @TODO: parse version names according to semantic versioning so they can be sorted properly
    tags :: !(HashMap Text NumberedVersion)
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

    pure $ Versions {master, tags}

data Master = Master
  { metadata :: !MasterMetadata,
    architectures :: !(HashMap Text ArchiveSpecification)
  }
  deriving (Eq, Show)

instance FromJSON Master where
  parseJSON value = do
    metadata <- JSON.parseJSON value
    architectures <- parseArchitectures value

    pure $ Master {metadata, architectures}

data NumberedVersion = NumberedVersion
  { metadata :: !NumberedVersionMetadata,
    architectures :: HashMap Text ArchiveSpecification
  }
  deriving (Eq, Show)

instance FromJSON NumberedVersion where
  parseJSON value = do
    metadata <- JSON.parseJSON value
    architectures <- parseArchitectures value

    pure $ NumberedVersion {metadata, architectures}

data MasterMetadata = MasterMetadata
  { version :: !String,
    date :: !Day,
    docs :: !Text,
    stdDocs :: !Text,
    src :: !ArchiveSpecification
  }
  deriving (Eq, Show, Generic, FromJSON)

data NumberedVersionMetadata = NumberedVersionMetadata
  { date :: !Day,
    docs :: !Text,
    notes :: !Text,
    stdDocs :: !(Maybe Text),
    src :: !ArchiveSpecification
  }
  deriving (Eq, Show, Generic, FromJSON)

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

foldMapM deriveJSON [''ArchitectureName, ''Command]

foldMapM deriveLensAndJSON [''Options, ''Settings]

foldMapM makeLenses [''App, ''ArchiveSpecification]

foldMapM makeWrapped [''Url]

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext
