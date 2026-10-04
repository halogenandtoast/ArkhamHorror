module Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV2 (bernardsFateV2) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype BernardsFateV2 = BernardsFateV2 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bernardsFateV2 :: StoryCard BernardsFateV2
bernardsFateV2 = story BernardsFateV2 Cards.bernardsFateV2

-- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'.
instance RunMessage BernardsFateV2 where
  runMessage msg s@(BernardsFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.siteOfAncientStones) reveal
      pure s
    _ -> BernardsFateV2 <$> liftRunMessage msg attrs
