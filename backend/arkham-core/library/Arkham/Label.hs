module Arkham.Label where

import Arkham.Prelude

newtype Label = Label { unLabel :: Text }
  deriving newtype (Show, Eq, ToJSON, FromJSON, Ord)

mkLabel :: Text -> Label
mkLabel = Label

class GetLabel env a where
  getLabel :: MonadReader env m => a -> m Label
