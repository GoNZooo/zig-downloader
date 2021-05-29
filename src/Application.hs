module Application where

import Data.List.Split (splitWhen)
import qualified HTTP
import Import
import Numeric (showFFloat)
import qualified RIO.Directory as Directory
import qualified RIO.HashMap as Map
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Text as Text
import System.FilePath ((</>))
import System.IO (putStr, putStrLn)
import Utilities (createIfNotExists, descending)
import ZigIndex (getMaster, getVersions)

showMaster :: RIO App ()
showMaster = do
  maybeMaster <- getMaster
  case maybeMaster of
    Right master -> showVersionData "Master" master
    Left e ->
      output $ "Unable to get master: " <> show e

showVersion :: Text -> RIO App ()
showVersion versionName = do
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {versionsTags} -> do
      case Map.lookup versionName versionsTags of
        Just version -> showVersionData versionName version
        Nothing -> printErrorForUnrecognizedTag versionsTags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

listVersions :: RIO App ()
listVersions = do
  maybeVersions <- getVersions
  case maybeVersions of
    Right
      Versions
        { versionsMaster = Version {versionMetadata = VersionMetadata {versionMetadataVersion}},
          versionsTags
        } ->
        do
          let tagKeys = versionsTags & Map.keys & List.sortBy descending

          loudOutput $ "Master version: " <> maybe "N/A" Text.unpack versionMetadataVersion
          output $ maybe "N/A" Text.unpack versionMetadataVersion

          loudOutputLine "Other versions:"
          liftIO $ forM_ tagKeys (Text.unpack >>> putStrLn)
    Left e ->
      logError $ "Unable to fetch versions: " <> fromString e

showVersionData :: Text -> Version -> RIO App ()
showVersionData
  versionName
  Version
    { versionMetadata =
        VersionMetadata
          { versionMetadataVersion,
            versionMetadataDate,
            versionMetadataDocs,
            versionMetadataStdDocs,
            versionMetadataSrc
          },
      versionArchitectures
    } = do
    quiet <- quietModeOn
    let outputString =
          if quiet
            then maybe "N/A" Text.unpack versionMetadataVersion
            else
              unlines
                [ Text.unpack versionName,
                  "------",
                  "Version: " <> maybe "N/A" Text.unpack versionMetadataVersion,
                  "Date: " <> Text.unpack versionMetadataDate,
                  "Docs: " <> Text.unpack versionMetadataDocs,
                  "Standard library docs: " <> maybe "N/A" Text.unpack versionMetadataStdDocs,
                  "Source: " <> tarballUrl,
                  "\nArchitectures",
                  "-------------"
                ]
        tarballUrl = versionMetadataSrc & archiveSpecificationTarball & unUrl
    output outputString
    forM_ (Map.toList versionArchitectures) printArchitecture

printArchitecture :: (Text, ArchiveSpecification) -> RIO App ()
printArchitecture (name, specification) = do
  quiet <- quietModeOn
  let megaBytesString = showFFloat (Just 2) (fromIntegral size / (1000000.0 :: Float)) ""
      size = archiveSpecificationSize specification
      architecture = Text.unpack name
      tarball' = specification & archiveSpecificationTarball & unUrl
      outputString =
        if quiet
          then tarball'
          else mconcat [architecture, ": ", tarball', " (", megaBytesString, " MB)"]
  liftIO $ putStrLn outputString

downloadVersion :: Text -> RIO App ()
downloadVersion "master" = do
  downloadPath <- getDownloadPath
  maybeMaster <- getMaster
  case maybeMaster of
    Right
      Version
        { versionMetadata = VersionMetadata {versionMetadataVersion},
          versionArchitectures
        } -> do
        let versionName = versionMetadataVersion & fromMaybe "master" & Text.unpack
        downloadArchitectures (downloadPath </> versionName) versionArchitectures
        setLatestMaster $ versionMetadataVersion & fromMaybe "master"
    Left errorText ->
      logError $ "Unable to get master: " <> fromString errorText
downloadVersion versionName = do
  downloadPath <- getDownloadPath
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {versionsTags} -> do
      case Map.lookup versionName versionsTags of
        Just Version {versionArchitectures} -> do
          downloadArchitectures (downloadPath </> Text.unpack versionName) versionArchitectures
        Nothing -> printErrorForUnrecognizedTag versionsTags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

setLatestMaster :: Text -> RIO App ()
setLatestMaster version = do
  downloadPath <- getDownloadPath
  writeFileUtf8 (downloadPath </> "latest-master") version

downloadArchitectures :: FilePath -> HashMap Text ArchiveSpecification -> RIO App ()
downloadArchitectures path = Map.toList >>> traverse_ (downloadArchitecture path)

downloadArchitecture :: FilePath -> (Text, ArchiveSpecification) -> RIO App ()
downloadArchitecture
  versionPath
  (architectureName, ArchiveSpecification {archiveSpecificationTarball}) = do
    let architecturePath = versionPath </> Text.unpack architectureName
        tarballUrlPaths = archiveSpecificationTarball & unUrl & splitWhen (== '/')
        tarballFilename = case tarballUrlPaths of
          [] -> error "Tarball URL is blank"
          other -> Partial.last other
        tarballPath = architecturePath </> tarballFilename
    liftIO $ createIfNotExists architecturePath
    downloadTarball archiveSpecificationTarball tarballPath

downloadTarball :: Url -> FilePath -> RIO App ()
downloadTarball (Url url) path = do
  alreadyHasTarball <- liftIO $ Directory.doesFileExist path
  if alreadyHasTarball
    then
      loudOutputLine $
        mconcat
          [ "Not downloading '",
            url,
            "' because it already exists in download directory."
          ]
    else do
      output $ "Downloading: " <> url
      HTTP.downloadBinaryFile (Url url) path

printErrorForUnrecognizedTag :: HashMap Text Version -> RIO App ()
printErrorForUnrecognizedTag tags = do
  let tagNames = tags & Map.keys & List.sortBy descending
  output "Unable to find version, available versions are:"
  liftIO $ forM_ tagNames (Text.unpack >>> putStrLn)

quietModeOn :: RIO App Bool
quietModeOn = do
  asks $ appOptions >>> optionsQuiet

loudOutput :: String -> RIO App ()
loudOutput text = do
  unlessM quietModeOn $ do
    liftIO $ putStr text

loudOutputLine :: String -> RIO App ()
loudOutputLine = (<> "\n") >>> loudOutput

output :: (MonadIO m) => String -> m ()
output = putStrLn >>> liftIO

getDownloadPath :: RIO App FilePath
getDownloadPath = do
  asks $ appOptions >>> optionsSettings >>> settingsDownloadPath
