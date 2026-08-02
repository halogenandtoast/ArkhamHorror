module Arkham.Story.Cards.HiddenVault (hiddenVault) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HiddenVault = HiddenVault StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenVault :: StoryCard HiddenVault
hiddenVault = story HiddenVault Cards.hiddenVault

instance RunMessage HiddenVault where
  runMessage msg s@(HiddenVault attrs) = runQueueT $ case msg of
    -- No flavor modal: the player reads this side off the flipped card itself.
    -- The location flips back and clears its flippable flag once this resolves.
    ResolveThisStory _ (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_u" :: Text, "Daughters" :: Text)
      pure s
    _ -> HiddenVault <$> liftRunMessage msg attrs
