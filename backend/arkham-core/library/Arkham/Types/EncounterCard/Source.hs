module Arkham.Types.EncounterCard.Source where

import Arkham.Prelude

data EncounterCardSource = FromDiscard | FromEncounterDeck | FromTheVoid
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
