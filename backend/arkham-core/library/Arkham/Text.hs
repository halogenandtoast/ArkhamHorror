module Arkham.Text where

import Arkham.Prelude

newtype Tooltip = Tooltip Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data FlavorText = FlavorText (Maybe Text) [Text]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
