module Arkham.Event.Cards.DaringManeuver
  ( daringManeuver
  , DaringManeuver(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Event.Helpers
import Arkham.Message
import Arkham.Target

newtype DaringManeuver = DaringManeuver EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daringManeuver :: EventCard DaringManeuver
daringManeuver = event DaringManeuver Cards.daringManeuver

instance RunMessage DaringManeuver where
  runMessage msg e@(DaringManeuver attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> e <$ pushAll
      [ skillTestModifier
        (toSource attrs)
        (InvestigatorTarget iid)
        (AnySkillValue 2)
      , discard attrs
      ]
    _ -> DaringManeuver <$> runMessage msg attrs
