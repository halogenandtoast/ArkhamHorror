module Arkham.Story.Cards.TheUnderseaVault (theUnderseaVault) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheUnderseaVault = TheUnderseaVault StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnderseaVault :: StoryCard TheUnderseaVault
theUnderseaVault = story TheUnderseaVault Cards.theUnderseaVault

instance RunMessage TheUnderseaVault where
  runMessage msg s@(TheUnderseaVault attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      campaignSpecific "translateGlyph" ("rune_z" :: Text, "City" :: Text)
      pure s
    _ -> TheUnderseaVault <$> liftRunMessage msg attrs
