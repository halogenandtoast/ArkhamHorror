module Arkham.Story.Cards.ObsidianRelic (obsidianRelic) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ObsidianRelic = ObsidianRelic StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianRelic :: StoryCard ObsidianRelic
obsidianRelic = story ObsidianRelic Cards.obsidianRelic

instance RunMessage ObsidianRelic where
  runMessage msg s@(ObsidianRelic attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_y" :: Text, "Knowledge" :: Text)
      -- "Add this card to the victory display" means this side, which is where
      -- the Victory 1 is printed; the asset front carries none. The asset side is
      -- the same physical card, so it leaves play rather than staying in front of
      -- its controller.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        AssetTarget aid -> removeFromGame aid
        _ -> pure ()
      pure s
    _ -> ObsidianRelic <$> liftRunMessage msg attrs
