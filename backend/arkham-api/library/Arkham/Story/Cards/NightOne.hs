module Arkham.Story.Cards.NightOne (nightOne) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype NightOne = NightOne StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightOne :: StoryCard NightOne
nightOne = story NightOne Cards.nightOne

instance RunMessage NightOne where
  runMessage msg s@(NightOne attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> NightOne <$> liftRunMessage msg attrs
