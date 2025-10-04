{-# LANGUAGE TemplateHaskell #-}

module Arkham.Investigator.Deck where

import Arkham.Prelude

import Data.Aeson.TH

data InvestigatorDeckKey = HunchDeck | UnderworldMarketDeck
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''InvestigatorDeckKey)

instance ToJSONKey InvestigatorDeckKey
instance FromJSONKey InvestigatorDeckKey
