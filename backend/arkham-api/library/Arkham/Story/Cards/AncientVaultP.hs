module Arkham.Story.Cards.AncientVaultP (ancientVaultP) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AncientVaultP = AncientVaultP StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultP :: StoryCard AncientVaultP
ancientVaultP = story AncientVaultP Cards.ancientVaultP

instance RunMessage AncientVaultP where
  runMessage msg s@(AncientVaultP attrs) = runQueueT $ case msg of
    -- The treachery reads this side with 'readStory', so it arrives focused in
    -- the UI and only resolves once the player clicks it to dismiss it.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_p" :: Text, "Weather" :: Text)
      -- "Add this card to the victory display" means this side, which is where the
      -- Victory 1 is printed. The treachery front is the same physical card, so it
      -- goes away entirely rather than being discarded.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        TreacheryTarget tid -> push $ QuietlyRemoveFromGame (TreacheryTarget tid)
        _ -> pure ()
      pure s
    _ -> AncientVaultP <$> liftRunMessage msg attrs
