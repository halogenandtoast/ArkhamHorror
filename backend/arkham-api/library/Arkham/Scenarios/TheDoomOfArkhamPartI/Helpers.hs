module Arkham.Scenarios.TheDoomOfArkhamPartI.Helpers where

import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.I18n
import Arkham.Prelude
import Data.Text qualified as T

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theDoomOfArkhamPartI" a

{- | Tillinghast Esoterica is "put into play adjacent to your location", and the
location it came from can be any of the six that hide a card, so the map carries
an empty slot beside each of them. A slot's label is the shop's label with the
hiding location's own label appended, which keeps the scenario layout and the
Revelation's placement in step without a lookup table to fall out of date.
-}
esotericaSlot :: Text -> Text
esotericaSlot lbl = "tillinghastEsoterica" <> T.toUpper (T.take 1 lbl) <> T.drop 1 lbl
