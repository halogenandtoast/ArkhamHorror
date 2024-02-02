module Arkham.Event.Cards.DaringManeuver2 (
  daringManeuver2,
  DaringManeuver2 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner

newtype DaringManeuver2 = DaringManeuver2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

daringManeuver2 :: EventCard DaringManeuver2
daringManeuver2 =
  event DaringManeuver2 Cards.daringManeuver2

instance RunMessage DaringManeuver2 where
  runMessage msg e@(DaringManeuver2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ drawing
        , skillTestModifier attrs iid (AnySkillValue 3)
        , RecalculateSkillTestResults
        ]
      pure e
    _ -> DaringManeuver2 <$> runMessage msg attrs
