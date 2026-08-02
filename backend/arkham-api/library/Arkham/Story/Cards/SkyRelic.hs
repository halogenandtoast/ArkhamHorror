module Arkham.Story.Cards.SkyRelic (skyRelic) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SkyRelic = SkyRelic StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skyRelic :: StoryCard SkyRelic
skyRelic = story SkyRelic Cards.skyRelicStory

instance RunMessage SkyRelic where
  runMessage msg s@(SkyRelic attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_f" :: Text, "Stars" :: Text)
      -- "Add this card to the victory display" means this side, which is where the
      -- Victory 1 is printed. The asset side is the same physical card, so it
      -- leaves play rather than staying in front of its controller — which is also
      -- what the front's "remove it from the game" clause amounts to here.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        AssetTarget aid -> removeFromGame aid
        _ -> pure ()
      pure s
    _ -> SkyRelic <$> liftRunMessage msg attrs
