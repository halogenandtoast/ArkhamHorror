module Arkham.Event.Events.DaringManeuver2 (daringManeuver2, DaringManeuver2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype DaringManeuver2 = DaringManeuver2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daringManeuver2 :: EventCard DaringManeuver2
daringManeuver2 = event DaringManeuver2 Cards.daringManeuver2

instance RunMessage DaringManeuver2 where
  runMessage msg e@(DaringManeuver2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      withSkillTest \sid -> do
        drawCardsIfCan iid attrs 1
        skillTestModifier sid attrs iid (AnySkillValue 3)
        push RecalculateSkillTestResults
      pure e
    _ -> DaringManeuver2 <$> liftRunMessage msg attrs
