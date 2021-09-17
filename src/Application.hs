module Application where

import Data.List.Split (splitWhen)
import Data.Time (showGregorian)
import Data.Time.Calendar (Day)
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

        outputIfLoud $ "Master version: " <> version
        output version

        outputLineIfLoud "Other versions:"
        liftIO $ forM_ tagKeys (Text.unpack >>> putStrLn)
    Left e ->
      logError $ "Unable to fetch versions: " <> fromString e

showMasterData :: Master -> RIO App ()
showMasterData
  Master
    { metadata = MasterMetadata {version, src, date, docs, stdDocs},
      architectures
    } = do
    output version
    outputLineIfLoud "------"
    outputLineIfLoud $ "Version: " <> version
    outputStandardVersionData date docs (Just stdDocs) (tarball src)
    outputArchitectureHeader
    forM_ (Map.toList architectures) printArchitecture

showVersionData :: Text -> NumberedVersion -> RIO App ()
showVersionData
  versionName
  NumberedVersion
    { metadata = NumberedVersionMetadata {src, date, docs, stdDocs},
      architectures
    } = do
    outputLineIfLoud $ Text.unpack versionName
    outputLineIfLoud "------"
    outputStandardVersionData date docs stdDocs (tarball src)
    outputArchitectureHeader
    forM_ (Map.toList architectures) printArchitecture

outputStandardVersionData :: Day -> Text -> Maybe Text -> Url -> RIO App ()
outputStandardVersionData date docs maybeStdDocs (Url tarball) = do
  outputLineIfLoud $ "Date: " <> showGregorian date
  outputLineIfLoud $ "Docs: " <> Text.unpack docs
  outputLineIfLoud $ "Standard library docs: " <> maybe "N/A" Text.unpack maybeStdDocs
  outputLineIfLoud $ "Source: " <> tarball

outputArchitectureHeader :: RIO App ()
outputArchitectureHeader = do
  outputLineIfLoud "\nArchitectures"
  outputLineIfLoud "-------------"

printArchitecture :: (Text, ArchiveSpecification) -> RIO App ()
printArchitecture (name, ArchiveSpecification {size, tarball = Url tarball}) = do
  quiet <- isQuietModeOn
  let megaBytesString = showFFloat (Just 2) (fromIntegral size / (1000000.0 :: Float)) ""
      architecture = Text.unpack name
      outputString =
        if quiet
          then tarball
          else mconcat [architecture, ": ", tarball, " (", megaBytesString, " MB)"]
  liftIO $ putStrLn outputString

downloadVersion :: Text -> [ArchitectureName] -> RIO App ()
downloadVersion "master" wantedArchitectures = do
  downloadPath <- getDownloadPath
  maybeMaster <- getMaster
  case maybeMaster of
    Right Master {metadata = MasterMetadata {version}, architectures} -> do
      let filteredArchitectures = existentArchitectures & fmap snd & mconcat
          (existentArchitectures, nonExistentArchitectures) =
            wantedArchitectures
              & fmap
                ( \(ArchitectureName n) ->
                    (n, Map.filterWithKey (\k _v -> k == n) architectures)
                )
              & List.partition (snd >>> Map.null >>> not)
          nonExistentNames = fmap fst nonExistentArchitectures
          availableArchitectures = Map.keys architectures
      downloadArchitectures (downloadPath </> version) filteredArchitectures
      warnAboutNonExistentArchitectures nonExistentNames availableArchitectures
      version & Text.pack & setLatestMaster
    Left errorText ->
      logError $ "Unable to get master: " <> fromString errorText
downloadVersion versionName wantedArchitectures = do
  downloadPath <- getDownloadPath
  maybeVersions <- getVersions
  case maybeVersions of
    Right Versions {tags} -> do
      case Map.lookup versionName tags of
        Just NumberedVersion {architectures} -> do
          let filteredArchitectures = existentArchitectures & fmap snd & mconcat
              (existentArchitectures, nonExistentArchitectures) =
                wantedArchitectures
                  & fmap
                    ( \(ArchitectureName n) ->
                        (n, Map.filterWithKey (\k _v -> k == n) architectures)
                    )
                  & List.partition (snd >>> Map.null >>> not)
              nonExistentNames = fmap fst nonExistentArchitectures
              availableArchitectures = Map.keys architectures
          downloadArchitectures (downloadPath </> Text.unpack versionName) filteredArchitectures
          warnAboutNonExistentArchitectures nonExistentNames availableArchitectures
        Nothing -> printErrorForUnrecognizedTag tags
    Left e ->
      logError $ "Unable to get versions: " <> fromString e

warnAboutNonExistentArchitectures :: [Text] -> [Text] -> RIO App ()
warnAboutNonExistentArchitectures [] _availableArchitectures =
  -- No warnings, do nothing
  mempty
warnAboutNonExistentArchitectures nonExistentArchitectures availableArchitectures = do
  let warnings =
        fmap (\a -> "Warning: Unable to find matches for '" <> a <> "'") nonExistentArchitectures
      availableArchitecturesString = Text.unpack $ Text.intercalate ", " availableArchitectures
  mapM_ (Text.unpack >>> output) warnings
  output $ "Available architectures: " <> availableArchitecturesString

matchingArchitectureName :: Text -> [ArchitectureName] -> Bool
matchingArchitectureName _architectureName [] = True
matchingArchitectureName architectureName architectures =
  any (`Text.isInfixOf` architectureName) (fmap unArchitectureName architectures)

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
      outputLineIfLoud $
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

isQuietModeOn :: RIO App Bool
isQuietModeOn = do
  asks $ appOptions >>> optionsQuiet

outputIfLoud :: String -> RIO App ()
outputIfLoud text = do
  unlessM isQuietModeOn $ do
    liftIO $ putStr text

outputLineIfLoud :: String -> RIO App ()
outputLineIfLoud = (<> "\n") >>> outputIfLoud

output :: (MonadIO m) => String -> m ()
output = putStrLn >>> liftIO

getDownloadPath :: RIO App FilePath
getDownloadPath = do
  asks $ appOptions >>> optionsSettings >>> settingsDownloadPath
