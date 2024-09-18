{-# LANGUAGE CPP #-}

module Import.NoFoundation (
  module Import.NoFoundation,
  module Import,
) where

import Control.Concurrent.STM.TChan as Import
import Database.Persist as Import hiding (get)
import Database.Persist qualified as P
import Model as Import
import Relude as Import
import Settings as Import
import Yesod.Core as Import
import Yesod.Core.Types as Import (loggerSet)
import Yesod.Persist.Core as Import hiding (YesodPersist (..))

import Data.Text qualified as T

tshow :: Show a => a -> Text
tshow = T.pack . show

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

get400
  :: (MonadHandler m, PersistStoreRead backend, PersistRecordBackend val backend)
  => Text
  -> Key val
  -> ReaderT backend m val
get400 msg key =
  P.get key >>= \case
    Nothing -> invalidArgs [msg]
    Just res -> pure res
