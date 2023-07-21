{-# LANGUAGE CPP #-}

module Import.NoFoundation (
  module Import.NoFoundation,
  module Import,
) where

import Control.Concurrent.STM.TChan as Import
import Database.Persist as Import hiding (get)
import Model as Import
import Relude as Import
import Settings as Import
import Yesod.Core as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Persist.Core as Import

import Data.Text qualified as T

tshow :: Show a => a -> Text
tshow = T.pack . show

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
