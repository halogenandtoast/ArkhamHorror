{-# LANGUAGE TemplateHaskell #-}
module Arkham.Investigator.Deck where

import Arkham.Prelude

import Data.Aeson.TH

data InvestigatorDeckKey = HunchDeck
  deriving stock (Show, Ord, Eq)

$(deriveJSON (defaultOptions { tagSingleConstructors = True}) ''InvestigatorDeckKey)

instance ToJSONKey InvestigatorDeckKey
instance FromJSONKey InvestigatorDeckKey
