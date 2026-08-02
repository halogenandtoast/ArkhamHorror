module Arkham.Story.Cards.AncientVaultO (ancientVaultO) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AncientVaultO = AncientVaultO StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientVaultO :: StoryCard AncientVaultO
ancientVaultO = story AncientVaultO Cards.ancientVaultO

instance RunMessage AncientVaultO where
  runMessage msg s@(AncientVaultO attrs) = runQueueT $ case msg of
    -- The treachery reads this side with 'readStory', so it arrives focused in
    -- the UI and only resolves once the player clicks it to dismiss it.
    ResolveThisStory iid (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_o" :: Text, "Power" :: Text)
      -- "Add this card to the victory display" means this side, which is where the
      -- Victory 1 is printed. The treachery front is the same physical card, so it
      -- goes away entirely rather than being discarded.
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        TreacheryTarget tid -> push $ QuietlyRemoveFromGame (TreacheryTarget tid)
        _ -> pure ()
      pure s
    _ -> AncientVaultO <$> liftRunMessage msg attrs
