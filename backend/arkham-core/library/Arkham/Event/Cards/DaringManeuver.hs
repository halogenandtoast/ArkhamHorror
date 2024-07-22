module Arkham.Event.Cards.DaringManeuver (daringManeuver, DaringManeuver (..)) where

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Prelude

newtype DaringManeuver = DaringManeuver EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daringManeuver :: EventCard DaringManeuver
daringManeuver = event DaringManeuver Cards.daringManeuver

instance RunMessage DaringManeuver where
  runMessage msg e@(DaringManeuver attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      withSkillTest \sid ->
        pushAll [skillTestModifier sid attrs iid (AnySkillValue 2), RecalculateSkillTestResults]
      pure e
    _ -> DaringManeuver <$> runMessage msg attrs
