module Arkham.Investigator.Deck where

import Arkham.Prelude

data InvestigatorDeckKey = HunchDeck
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Hashable, ToJSONKey, FromJSONKey)

instance ToJSON InvestigatorDeckKey where
  toJSON = genericToJSON $ defaultOptions { tagSingleConstructors = True }

instance FromJSON InvestigatorDeckKey where
  parseJSON = genericParseJSON $ defaultOptions { tagSingleConstructors = True }
