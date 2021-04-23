{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Import
import Options.Applicative.Simple
import qualified Paths_zig_downloader
import RIO.Process
import Run
import System.Directory
import System.IO (print)

main :: IO ()
main = do
  yamlConfigurationPath <- getXdgDirectory XdgConfig "zig-downloader/config.yaml"
  maybeYamlOptions <- decodeFileEither yamlConfigurationPath
  let defaultDownloadPath = case maybeYamlOptions of
        Right Options {optionsDownloadPath = path} -> path
        Left errorString -> impureThrow errorString

  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_zig_downloader.version)
      "zig-downloader"
      "Downloader for Zig pre-built binaries"
      ( Options
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
      )
      empty
  print options
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
