module Run (run) where

import Application
import Import

run :: Command -> RIO App ()
run ListCommand = listVersions
run (ShowCommand "master") = showMaster
run (ShowCommand versionName) = showVersion versionName
run (DownloadCommand "master") = downloadVersion "master"
run (DownloadCommand versionName) = downloadVersion versionName
