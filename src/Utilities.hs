module Utilities where

import RIO
import qualified RIO.Directory as Directory
import qualified RIO.HashMap as Map

deleteAllKeys :: (Eq k) => [k] -> HashMap k v -> HashMap k v
deleteAllKeys keys = Map.filterWithKey (\k _v -> k `notElem` keys)

descending :: (Ord a) => a -> a -> Ordering
descending = flip compare

createIfNotExists :: FilePath -> IO ()
createIfNotExists = Directory.createDirectoryIfMissing True
