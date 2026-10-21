module Arkham.Story.Cards.TracesToNowhere.AcrossTheThreshold (acrossTheThreshold) where

import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype AcrossTheThreshold = AcrossTheThreshold StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrossTheThreshold :: StoryCard AcrossTheThreshold
acrossTheThreshold = story AcrossTheThreshold Cards.acrossTheThreshold

instance RunMessage AcrossTheThreshold where
  runMessage msg s@(AcrossTheThreshold attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      descendToEtherealPerch attrs Locations.meigerEstateBeyondTheBlackDoor
      remember TheInvestigatorsGlimpsedAnotherWorld
      pure s
    _ -> AcrossTheThreshold <$> liftRunMessage msg attrs
