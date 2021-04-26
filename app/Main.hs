{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Import
import Options.Applicative.Simple
import qualified Paths_zig_downloader
import RIO.Process
import Run
import System.Directory

main :: IO ()
main = do
  yamlConfigurationPath <- getXdgDirectory XdgConfig "zig-downloader/config.yaml"
  maybeYamlOptions <- decodeFileEither yamlConfigurationPath
  let defaultDownloadPath = case maybeYamlOptions of
        Right Settings {settingsDownloadPath = path} -> path
        Left errorString -> impureThrow errorString

  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_zig_downloader.version)
      "zig-downloader"
      "Downloader for Zig pre-built binaries"
      ( Options
          <$> parseSettings defaultDownloadPath
          <*> subparser
            ( command
                "show"
                ( info
                    parseShowCommand
                    (progDesc "Show a version, or master if no argument is given")
                )
                <> command "list" (info (pure ListCommand) (progDesc "List all versions"))
                <> command "download" (info parseDownloadCommand (progDesc "Download a given version"))
            )
      )
      empty
  lo <- logOptionsHandle stderr (options & optionsSettings & settingsVerbose)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app (run $ optionsCommand options)

parseSettings :: Text -> Parser Settings
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
parseDownloadCommand = DownloadCommand <$> argument str (metavar "VERSION")
