module Arkham.Event.Events.DaringManeuver (daringManeuver, DaringManeuver (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype DaringManeuver = DaringManeuver EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daringManeuver :: EventCard DaringManeuver
daringManeuver = event DaringManeuver Cards.daringManeuver

instance RunMessage DaringManeuver where
  runMessage msg e@(DaringManeuver attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs iid (AnySkillValue 2)
        push RecalculateSkillTestResults
      pure e
    _ -> DaringManeuver <$> liftRunMessage msg attrs
