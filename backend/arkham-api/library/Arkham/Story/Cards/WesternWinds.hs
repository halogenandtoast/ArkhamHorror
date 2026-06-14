module Arkham.Story.Cards.WesternWinds (westernWinds) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype WesternWinds = WesternWinds StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernWinds :: StoryCard WesternWinds
westernWinds = story WesternWinds Cards.westernWinds

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage WesternWinds where
  runMessage msg (WesternWinds attrs) = runQueueT $ WesternWinds <$> liftRunMessage msg attrs
