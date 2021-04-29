module Run (run) where

import Import
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Numeric
import qualified RIO.Directory as Directory
import qualified RIO.HashMap as Map
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Text as Text
import System.FilePath ((</>))
import System.IO (putStrLn, putStr)
import Utilities (descending)
import ZigIndex

run :: Command -> RIO App ()
run ListCommand = do
  quietMode <- asks (optionsQuiet . appOptions)
  maybeVersions <- liftIO getVersions
  case maybeVersions of
    Right
      Versions
        { versionsMaster =
            Version {versionMetadata = VersionMetadata {versionMetadataVersion}},
          versionsTags
        } ->
        do
          let tagKeys = versionsTags & Map.keys & List.sortBy descending

          unless quietMode $ do
            liftIO $ putStr $ "Master version: " <> maybe "N/A" Text.unpack versionMetadataVersion
          liftIO $ putStrLn $ maybe "N/A" Text.unpack versionMetadataVersion

          unless quietMode $ do
            liftIO $ putStrLn "Other versions:"
          liftIO $ forM_ tagKeys (putStrLn . Text.unpack)
    Left e ->
      logError $ "Unable to fetch versions: " <> fromString e
run (ShowCommand "master") = do
  maybeMaster <- liftIO getMaster
  case maybeMaster of
    Right master -> liftIO $ showVersion "Master" master
    Left e ->
      liftIO $ putStrLn $ "Unable to get master: " <> show e
run (ShowCommand versionName) = do
  maybeVersions <- liftIO getVersions
  case maybeVersions of
    Right Versions {versionsTags} -> do
      case Map.lookup versionName versionsTags of
        Just version -> liftIO $ showVersion versionName version
        Nothing -> liftIO $ printErrorForUnrecognizedTag versionsTags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e
run (DownloadCommand "master") = do
  downloadPath <- asks (settingsDownloadPath . optionsSettings . appOptions)
  maybeMaster <- liftIO getMaster
  case maybeMaster of
    Right master@Version {versionMetadata} -> do
      let versionName = fromMaybe "master" $ versionMetadataVersion versionMetadata
      liftIO $ downloadVersion downloadPath versionName master
    Left e ->
      logError $ "Unable to get versions: " <> fromString e
run (DownloadCommand versionName) = do
  downloadPath <- asks (settingsDownloadPath . optionsSettings . appOptions)
  maybeVersions <- liftIO getVersions
  case maybeVersions of
    Right Versions {versionsTags} -> do
      case Map.lookup versionName versionsTags of
        Just version -> liftIO $ downloadVersion downloadPath versionName version
        Nothing -> liftIO $ printErrorForUnrecognizedTag versionsTags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

showVersion :: Text -> Version -> IO ()
showVersion
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
      versionArchitectures = architectures
    } = do
    putStrLn $ Text.unpack versionName
    putStrLn "------"
    putStrLn $ "Version: " <> maybe "N/A" Text.unpack versionMetadataVersion
    putStrLn $ "Date: " <> Text.unpack versionMetadataDate
    putStrLn $ "Docs: " <> Text.unpack versionMetadataDocs
    putStrLn $ "Standard library docs: " <> maybe "N/A" Text.unpack versionMetadataStdDocs
    putStrLn $ "Source: " <> Text.unpack (archiveSpecificationTarball versionMetadataSrc)
    putStrLn "\nArchitectures"
    putStrLn "-------------"

    forM_ (Map.toList architectures) printArchitecture

printArchitecture :: (Text, ArchiveSpecification) -> IO ()
printArchitecture
  ( architectureName,
    ArchiveSpecification
      { archiveSpecificationTarball,
        archiveSpecificationSize
      }
    ) =
    let megaBytesString =
          showFFloat @Float (Just 2) (fromIntegral archiveSpecificationSize / 1000000.0) ""
        architecture' = Text.unpack architectureName
        tarball' = Text.unpack archiveSpecificationTarball
     in putStrLn $ mconcat [architecture', ": ", tarball', " (", megaBytesString, " MB)"]

downloadVersion :: Text -> Text -> Version -> IO ()
downloadVersion
  downloadPath
  versionName
  Version
    { versionMetadata = VersionMetadata {versionMetadataVersion},
      versionArchitectures
    } = do
    let versionName' = maybe (Text.unpack versionName) Text.unpack versionMetadataVersion
        downloadPath' = Text.unpack downloadPath
        versionPath = downloadPath' </> versionName'
    versionArchitectures & Map.toList & traverse_ (downloadArchitecture versionPath)

createIfNotExists :: FilePath -> IO ()
createIfNotExists = Directory.createDirectoryIfMissing True

downloadArchitecture :: FilePath -> (Text, ArchiveSpecification) -> IO ()
downloadArchitecture
  versionPath
  (architectureName, ArchiveSpecification {archiveSpecificationTarball}) = do
    let architecturePath = versionPath </> Text.unpack architectureName
        tarballUrl = Text.unpack archiveSpecificationTarball
        tarballFilename = case archiveSpecificationTarball & Text.split (== '/') of
          [] -> error "Tarball URL is blank"
          other -> Partial.last other & Text.unpack
        tarballPath = architecturePath </> tarballFilename
    createIfNotExists architecturePath
    doesTarballExist <- Directory.doesFileExist tarballPath
    if not doesTarballExist
      then do
        putStrLn $ "Downloading: " <> tarballUrl
        manager <- newTlsManager
        request <- parseRequest tarballUrl
        response <- httpLbs request manager
        let body = responseBody response
        writeFileBinary tarballPath $ toStrictBytes body
      else pure ()

printErrorForUnrecognizedTag :: HashMap Text Version -> IO ()
printErrorForUnrecognizedTag tags = do
  let tagNames = tags & Map.keys & List.sortBy descending
  liftIO $ putStrLn "Unable to find version, available versions are:"
  liftIO $ forM_ tagNames (putStrLn . Text.unpack)