module Arkham.Story.Cards.TracesToNowhere.PulledBelow (pulledBelow) where

import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype PulledBelow = PulledBelow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledBelow :: StoryCard PulledBelow
pulledBelow = story PulledBelow Cards.pulledBelow

instance RunMessage PulledBelow where
  runMessage msg s@(PulledBelow attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      descendToEtherealPerch attrs Locations.duterteFuneralHomeSomethingInTheBasement
      pure s
    _ -> PulledBelow <$> liftRunMessage msg attrs
