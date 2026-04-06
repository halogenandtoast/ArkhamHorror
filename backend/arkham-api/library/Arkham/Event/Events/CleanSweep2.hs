module Arkham.Event.Events.CleanSweep2 (cleanSweep2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.SkillTest.Lifted (investigateWith_)
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype CleanSweep2 = CleanSweep2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanSweep2 :: EventCard CleanSweep2
cleanSweep2 = event CleanSweep2 Cards.cleanSweep2

instance RunMessage CleanSweep2 where
  runMessage msg e@(CleanSweep2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #intellect)
      investigateWith_ #agility sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      locations <- getAccessibleLocations iid attrs
      chooseTargetM iid locations $ moveTo attrs iid
      pure e
    _ -> CleanSweep2 <$> liftRunMessage msg attrs
