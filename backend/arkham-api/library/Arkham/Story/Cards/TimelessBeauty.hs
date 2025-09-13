module Arkham.Story.Cards.TimelessBeauty (timelessBeauty) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TimelessBeauty = TimelessBeauty StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelessBeauty :: StoryCard TimelessBeauty
timelessBeauty = story TimelessBeauty Cards.timelessBeauty

instance RunMessage TimelessBeauty where
  runMessage msg s@(TimelessBeauty attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      iids <- select $ InvestigatorAt (locationIs Locations.serannian)
      for_ iids \iid -> do
        gainResources iid attrs 2
        healDamageIfCan iid attrs 2
      pure s
    _ -> TimelessBeauty <$> liftRunMessage msg attrs
