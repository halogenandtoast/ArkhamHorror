{- | Shared rules for Consternation on the Constellation.

The scenario's own mechanic is the __exhausted location__: a sinking ship takes
on water deck by deck, and an exhausted location is one that has flooded. Taking
on Water and agenda 3b exhaust "a ready location with the lowest possible @Deck@
number", locations do not ready during upkeep once agenda 3 is out, and a dozen
cards read the state. Locations exhaust and ready through the engine's own
exhaust mechanics, so there is nothing to model here beyond the ordering rule.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.Helpers where

import Arkham.I18n
import Arkham.Prelude

-- * Text

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "consternationOnTheConstellation" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
