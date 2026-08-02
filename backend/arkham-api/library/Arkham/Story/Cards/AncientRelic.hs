module Arkham.Story.Cards.AncientRelic (ancientRelic) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AncientRelic = AncientRelic StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientRelic :: StoryCard AncientRelic
ancientRelic = story AncientRelic Cards.ancientRelic

instance RunMessage AncientRelic where
  runMessage msg s@(AncientRelic attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_s" :: Text, "Machine" :: Text)
      -- "Add this card to the victory display" means this side, where the Victory 1
      -- is printed. The asset front is the same physical card, so it goes away
      -- entirely rather than being discarded or set aside.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        AssetTarget aid -> push $ QuietlyRemoveFromGame (AssetTarget aid)
        _ -> pure ()
      pure s
    _ -> AncientRelic <$> liftRunMessage msg attrs
