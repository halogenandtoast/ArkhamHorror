module Cards.Discover
  ( findCardFiles
  ) where

import Cards.Discover.Exe (getFilesRecursive, stripSuffix)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (makeRelativeToProject)
import Prelude

findCardFiles :: FilePath -> Code Q [FilePath]
findCardFiles root = liftCode $ do
  projectRoot <- makeRelativeToProject root
  files <- runIO $ filter isHaskellFile <$> getFilesRecursive projectRoot
  examineCode $ liftTyped files
 where
  isHaskellFile filename = case stripSuffix ".hs" filename of
    Just _ -> True
    _ -> False
