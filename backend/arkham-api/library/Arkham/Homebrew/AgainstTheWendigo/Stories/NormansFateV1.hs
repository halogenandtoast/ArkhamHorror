module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV1 (normansFateV1) where

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

newtype NormansFateV1 = NormansFateV1 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. In this
version the swamp ate Norman; the card flips into what is still in there.
-}
normansFateV1 :: StoryCard NormansFateV1
normansFateV1 = persistStory $ story NormansFateV1 Cards.normansFateV1

instance RunMessage NormansFateV1 where
  runMessage msg s@(NormansFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.swamp) reveal
      -- Put aside, but still on the table: the Swamp's reaction reads
      -- the second part by resolving this card again.
      pure $ NormansFateV1 $ attrs & placementL .~ Global
    -- The second part, asked for by the Swamp once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredNormansFate
      lead <- getLead
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      drawCard lead =<< genCard Treacheries.manEaters
      pure s
    _ -> NormansFateV1 <$> liftRunMessage msg attrs
