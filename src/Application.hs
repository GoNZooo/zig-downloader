module Application where

import Data.List.Split (splitWhen)
import Data.Time (showGregorian)
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
    Right master -> showMasterData master
    Left e ->
      output $ "Unable to get master: " <> show e

showVersion :: Text -> RIO App ()
showVersion versionName = do
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {tags} -> do
      case Map.lookup versionName tags of
        Just version -> showVersionData versionName version
        Nothing -> printErrorForUnrecognizedTag tags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

listVersions :: RIO App ()
listVersions = do
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {master = Master {metadata = MasterMetadata {version}}, tags} ->
      do
        let tagKeys = tags & Map.keys & List.sortBy descending

        loudOutput $ "Master version: " <> version
        output version

        loudOutputLine "Other versions:"
        liftIO $ forM_ tagKeys (Text.unpack >>> putStrLn)
    Left e ->
      logError $ "Unable to fetch versions: " <> fromString e

showMasterData :: Master -> RIO App ()
showMasterData
  Master
    { metadata = MasterMetadata {version, src, date, docs, stdDocs},
      architectures
    } = do
    quiet <- quietModeOn
    if quiet
      then do
        output version
      else do
        let tarballUrl = src & tarball & unUrl
        output version
        output "------"
        output $ "Version: " <> version
        output $ "Date: " <> showGregorian date
        output $ "Docs: " <> Text.unpack docs
        output $ "Standard library docs: " <> Text.unpack stdDocs
        output $ "Source: " <> tarballUrl
        output "\nArchitectures"
        output "-------------"
    forM_ (Map.toList architectures) printArchitecture

showVersionData :: Text -> NumberedVersion -> RIO App ()
showVersionData
  versionName
  NumberedVersion
    { metadata = NumberedVersionMetadata {src, date, docs, stdDocs},
      architectures
    } = do
    quiet <- quietModeOn
    unless quiet $ do
      let tarballUrl = src & tarball & unUrl
      output $ Text.unpack versionName
      output "------"
      output $ "Date: " <> showGregorian date
      output $ "Docs: " <> Text.unpack docs
      output $ "Standard library docs: " <> maybe "N/A" Text.unpack stdDocs
      output $ "Source: " <> tarballUrl
      output "\nArchitectures"
      output "-------------"
    forM_ (Map.toList architectures) printArchitecture

printArchitecture :: (Text, ArchiveSpecification) -> RIO App ()
printArchitecture (name, ArchiveSpecification {size, tarball = Url tarball}) = do
  quiet <- quietModeOn
  let megaBytesString = showFFloat (Just 2) (fromIntegral size / (1000000.0 :: Float)) ""
      architecture = Text.unpack name
      outputString =
        if quiet
          then tarball
          else mconcat [architecture, ": ", tarball, " (", megaBytesString, " MB)"]
  liftIO $ putStrLn outputString

downloadVersion :: Text -> RIO App ()
downloadVersion "master" = do
  downloadPath <- getDownloadPath
  maybeMaster <- getMaster
  case maybeMaster of
    Right Master {metadata = MasterMetadata {version}, architectures} -> do
      downloadArchitectures (downloadPath </> version) architectures
      version & Text.pack & setLatestMaster
    Left errorText ->
      logError $ "Unable to get master: " <> fromString errorText
downloadVersion versionName = do
  downloadPath <- getDownloadPath
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {tags} -> do
      case Map.lookup versionName tags of
        Just NumberedVersion {architectures} -> do
          downloadArchitectures (downloadPath </> Text.unpack versionName) architectures
        Nothing -> printErrorForUnrecognizedTag tags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

setLatestMaster :: Text -> RIO App ()
setLatestMaster version = do
  downloadPath <- getDownloadPath
  writeFileUtf8 (downloadPath </> "latest-master") version

downloadArchitectures :: FilePath -> HashMap Text ArchiveSpecification -> RIO App ()
downloadArchitectures path = Map.toList >>> traverse_ (downloadArchitecture path)

downloadArchitecture :: FilePath -> (Text, ArchiveSpecification) -> RIO App ()
downloadArchitecture versionPath (architectureName, ArchiveSpecification {tarball}) = do
  let architecturePath = versionPath </> Text.unpack architectureName
      tarballUrlPaths = tarball & unUrl & splitWhen (== '/')
      tarballFilename = case tarballUrlPaths of
        [] -> error "Tarball URL is blank"
        other -> Partial.last other
      tarballPath = architecturePath </> tarballFilename
  liftIO $ createIfNotExists architecturePath
  downloadTarball tarball tarballPath

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

printErrorForUnrecognizedTag :: HashMap Text NumberedVersion -> RIO App ()
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
