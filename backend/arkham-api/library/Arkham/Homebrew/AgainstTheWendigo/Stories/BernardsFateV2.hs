module Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV2 (bernardsFateV2) where

import Arkham.Card (genCard)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype BernardsFateV2 = BernardsFateV2 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. In this
version Bernard did not come back at all; the card flips into the clearing
where he was killed.
-}
bernardsFateV2 :: StoryCard BernardsFateV2
bernardsFateV2 = persistStory $ story BernardsFateV2 Cards.bernardsFateV2

instance RunMessage BernardsFateV2 where
  runMessage msg s@(BernardsFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.siteOfAncientStones) reveal
      -- Put aside, but still on the table: the Site of Ancient Stones's reaction reads
      -- the second part by resolving this card again.
      pure $ BernardsFateV2 $ attrs & placementL .~ Global
    -- The second part, asked for by the Site of Ancient Stones once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredBernardsFate
      lead <- getLead
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      drawCard lead =<< genCard Treacheries.theClearingOfTheSacrifices
      pure s
    _ -> BernardsFateV2 <$> liftRunMessage msg attrs
