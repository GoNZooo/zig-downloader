module Run (run) where

import Application
import Import

run :: Command -> RIO App ()
run ListCommand = listVersions
run (ShowCommand "master") = showMaster
run (ShowCommand versionName) = showVersion versionName
run (DownloadCommand "master" architectures) = downloadVersion "master" architectures
run (DownloadCommand versionName architectures) = downloadVersion versionName architectures
