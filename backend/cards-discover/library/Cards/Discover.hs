module Cards.Discover
  ( findCardFiles
  ) where

import Cards.Discover.Exe (getFilesRecursive, stripSuffix)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude

findCardFiles :: FilePath -> Q (TExp [FilePath])
findCardFiles root = do
  projectRoot <- makeRelativeToProject root
  files <- runIO $ filter isHaskellFile <$> getFilesRecursive projectRoot
  liftTyped files
 where
  isHaskellFile filename = case stripSuffix ".hs" filename of
    Just _ -> True
    _ -> False
