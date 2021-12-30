module Arkham.Label where

import Arkham.Prelude

newtype Label = Label { unLabel :: Text }

class GetLabel env a where
  getLabel :: MonadReader env m => a -> m Label
