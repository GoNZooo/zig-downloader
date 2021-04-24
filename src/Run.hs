{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified RIO.Directory as Directory
import qualified RIO.HashMap as Map
import qualified RIO.List as List
import qualified RIO.List.Partial as Partial
import qualified RIO.Text as Text
import System.FilePath ((</>))
import System.IO (putStrLn)
import Utilities (descending)
import ZigIndex

run :: Command -> RIO App ()
run ListCommand = do
  maybeVersions <- liftIO getVersions
  case maybeVersions of
    Right
      Versions
        { versionsMaster =
            Version {versionMetadata = VersionMetadata {versionMetadataVersion = masterVersion}},
          versionsTags = tags
        } ->
        do
          let tagKeys = tags & Map.keys & List.sortBy descending
          liftIO $ putStrLn $ "Master version: " <> maybe "N/A" Text.unpack masterVersion
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
    Right Versions {versionsTags = tags} -> do
      case Map.lookup versionName tags of
        Just version -> liftIO $ showVersion versionName version
        Nothing -> do
          let tagNames = tags & Map.keys & List.sortBy descending
          liftIO $ putStrLn "Unable to find version, available versions are:"
          liftIO $ forM_ tagNames (putStrLn . Text.unpack)
    Left e ->
      logError $ "Unable to get versions: " <> fromString e
run (DownloadCommand "master") = do
  downloadPath <- asks (settingsDownloadPath . optionsSettings . appOptions)
  maybeMaster <- liftIO getMaster
  case maybeMaster of
    Right master@Version {versionMetadata = meta} -> do
      let versionName = fromMaybe "master" $ versionMetadataVersion meta
      liftIO $ downloadVersion downloadPath versionName master
    Left e ->
      logError $ "Unable to get versions: " <> fromString e
run (DownloadCommand versionName) = do
  downloadPath <- asks (settingsDownloadPath . optionsSettings . appOptions)
  maybeVersions <- liftIO getVersions
  case maybeVersions of
    Right Versions {versionsTags = tags} -> do
      case Map.lookup versionName tags of
        Just version -> liftIO $ downloadVersion downloadPath versionName version
        Nothing -> do
          let tagNames = tags & Map.keys & List.sortBy descending
          liftIO $ putStrLn "Unable to find version, available versions are:"
          liftIO $ forM_ tagNames (putStrLn . Text.unpack)
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

showVersion :: Text -> Version -> IO ()
showVersion
  versionName
  Version
    { versionMetadata =
        VersionMetadata
          { versionMetadataVersion = maybeVersion,
            versionMetadataDate = date,
            versionMetadataDocs = docs,
            versionMetadataStdDocs = maybeStdDocs,
            versionMetadataSrc = src
          },
      versionArchitectures = architectures
    } = do
    putStrLn $ Text.unpack versionName
    putStrLn "------"
    putStrLn $ "Version: " <> maybe "N/A" Text.unpack maybeVersion
    putStrLn $ "Date: " <> Text.unpack date
    putStrLn $ "Docs: " <> Text.unpack docs
    putStrLn $ "Standard library docs: " <> maybe "N/A" Text.unpack maybeStdDocs
    putStrLn $ "Source: " <> Text.unpack (archiveSpecificationTarball src)
    putStrLn "\nArchitectures"
    putStrLn "-------------"

    forM_ (Map.toList architectures) $
      \( architectureName,
         ArchiveSpecification
           { archiveSpecificationTarball = tarball,
             archiveSpecificationSize = size
           }
         ) ->
          putStrLn $ Text.unpack $ mconcat [architectureName, ": ", tarball, " (", tshow size, " b)"]

downloadVersion :: Text -> Text -> Version -> IO ()
downloadVersion
  downloadPath
  versionName
  Version
    { versionMetadata = VersionMetadata {versionMetadataVersion = maybeVersion},
      versionArchitectures = architectures
    } = do
    let versionName' = maybe (Text.unpack versionName) Text.unpack maybeVersion
        downloadPath' = Text.unpack downloadPath
        versionPath = downloadPath' </> versionName'
    architectures & Map.toList & traverse_ (downloadArchitecture versionPath)

createIfNotExists :: FilePath -> IO ()
createIfNotExists = Directory.createDirectoryIfMissing True

downloadArchitecture :: FilePath -> (Text, ArchiveSpecification) -> IO ()
downloadArchitecture
  versionPath
  (architectureName, ArchiveSpecification {archiveSpecificationTarball = tarball}) = do
    let architecturePath = versionPath </> Text.unpack architectureName
        tarballUrl = Text.unpack tarball
        tarballFilename = case tarball & Text.split (== '/') of
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
