{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Import
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative.Simple
import qualified Paths_zig_downloader
import RIO.Process
import qualified RIO.Text as Text
import Run
import System.Directory

main :: IO ()
main = do
  yamlConfigurationPath <- getXdgDirectory XdgConfig "zig-downloader/config.yaml"
  maybeYamlOptions <- decodeFileEither yamlConfigurationPath
  let defaultDownloadPath = case maybeYamlOptions of
        Right settings -> settings ^. settingsDownloadPath
        Left errorString -> throwM $ YamlFileDecodingError errorString
      listCommand = command "list" $ info (pure ListCommand) (progDesc "List all versions")
      downloadCommand =
        command "download" (info parseDownloadCommand (progDesc "Download a given version"))
      showCommand =
        command "show" $
          info parseShowCommand (progDesc "Show a version")

  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_zig_downloader.version)
      "zig-downloader"
      "Downloader for Zig pre-built binaries"
      ( Options
          <$> parseSettings defaultDownloadPath
          <*> subparser (showCommand <> listCommand <> downloadCommand)
          <*> switch (long "quiet" <> short 'q' <> help "Whether to print less output or not")
      )
      empty
  lo <- logOptionsHandle stderr $ options ^. optionsSettings . settingsVerbose
  pc <- mkDefaultProcessContext
  tlsManager <- newTlsManager
  withLogFunc lo $ \lf ->
    let app =
          App
            { _appLogFunc = lf,
              _appProcessContext = pc,
              _appOptions = options,
              _appTlsManager = tlsManager
            }
     in runRIO app $ run $ options ^. optionsCommand

parseSettings :: FilePath -> Parser Settings
parseSettings defaultDownloadPath =
  Settings
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output?"
      )
    <*> option
      str
      ( long "path"
          <> short 'p'
          <> help "Path to download archives to"
          <> value defaultDownloadPath
      )
    <*> switch
      ( long "master"
          <> short 'm'
          <> help "Whether or not to download master version"
      )

parseShowCommand :: Parser Command
parseShowCommand = ShowCommand <$> argument str (metavar "VERSION")

parseDownloadCommand :: Parser Command
parseDownloadCommand =
  DownloadCommand
    <$> argument str (metavar "VERSION")
    <*> option
      (maybeReader maybeReadArchitectures)
      (long "architectures" <> short 'a' <> help "Architectures to download" <> value [])

maybeReadArchitectures :: String -> Maybe [ArchitectureName]
maybeReadArchitectures =
  Text.pack >>> Text.split (== ',') >>> fmap ArchitectureName >>> Just
