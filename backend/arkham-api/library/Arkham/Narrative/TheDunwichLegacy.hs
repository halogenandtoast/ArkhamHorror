{-# LANGUAGE ImplicitParams #-}

{- | The narrative variants written for The Dunwich Legacy.

This is the one place a variant is named. The campaign and its scenarios resolve
whichever one the game was created with and read beats off it, so adding a
variant means adding a line here and nothing else.
-}
module Arkham.Narrative.TheDunwichLegacy (
  module Arkham.Narrative.TheDunwichLegacy.Types,
  addsFalseLeads,
  getDunwichNarrative,
) where

import Arkham.Campaign.Option
import Arkham.Helpers.Narrative
import Arkham.I18n
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Narrative.OlausWormius.TheDunwichLegacy qualified as OlausWormius
import Arkham.Narrative.TheDunwichLegacy.Types
import Arkham.Prelude

narratives :: (HasI18n, ReverseQueue m) => [(Text, DunwichNarrative m)]
narratives = [(OlausWormius.variant, OlausWormius.narrative)]

{- | The narrative in play, or the Campaign Guide's own.

Resolved where it is used, so the beats it hands back are built under that site's
i18n scope.
-}
getDunwichNarrative :: (HasI18n, ReverseQueue m) => m (DunwichNarrative m)
getDunwichNarrative = do
  active <- getNarrative
  pure $ fromMaybe noDunwichNarrative (active >>= (`lookup` narratives))

{- | Whether the narrative a game was created with adds "Interlude I-A: False
Leads" between Scenarios II and III.

A step, not a beat: it changes the campaign's shape rather than a block's prose,
so 'Arkham.Campaign.Types.IsCampaign.nextStep' asks for it from the log's options
alone.
-}
addsFalseLeads :: Set CampaignOption -> Bool
addsFalseLeads options = narrativeOf options == Just OlausWormius.variant
