{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as JSON
import RIO
import RIO.Process
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as PartialText
import Text.Inflections

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsDownloadPath :: !Text
  }
  deriving (Show, Generic)

parseJSONOptions :: String -> JSON.Options
parseJSONOptions prefix =
  JSON.defaultOptions {JSON.fieldLabelModifier = removePrefix prefix}

instance FromJSON Options where
  parseJSON = JSON.genericParseJSON $ parseJSONOptions "options"

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
