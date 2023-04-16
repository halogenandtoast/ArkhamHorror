module Arkham.Text where

import Arkham.Prelude
import Arkham.Json

newtype Tooltip = Tooltip Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data FlavorText = FlavorText
  { flavorTitle :: Maybe Text
  , flavorBody :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup FlavorText where
  FlavorText mTitle1 body1 <> FlavorText mTitle2 body2 = FlavorText (mTitle1 <|> mTitle2) (body1 <> body2)

instance Monoid FlavorText where
  mempty = FlavorText Nothing []

instance IsString FlavorText where
  fromString s = FlavorText Nothing [fromString s]

instance ToJSON FlavorText where
  toJSON = genericToJSON $ aesonOptions $ Just "flavor"

instance FromJSON FlavorText where
  parseJSON = genericParseJSON $ aesonOptions $ Just "flavor"
