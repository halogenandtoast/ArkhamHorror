module Arkham.Helpers.Card
  ( module Arkham.Helpers.Card
  , module Arkham.Helpers.Campaign
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Helpers.Campaign

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
