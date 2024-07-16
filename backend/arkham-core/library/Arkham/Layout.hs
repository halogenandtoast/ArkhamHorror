module Arkham.Layout where

import Arkham.Prelude

newtype GridTemplateRow = GridTemplateRow {unGridTemplateRow :: Text}
  deriving stock Data
  deriving newtype (Show, IsString, ToJSON, FromJSON, Eq)
