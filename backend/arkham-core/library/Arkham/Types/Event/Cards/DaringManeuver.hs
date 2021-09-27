module Arkham.Types.Event.Cards.DaringManeuver
  ( daringManeuver
  , DaringManeuver(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype DaringManeuver = DaringManeuver EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daringManeuver :: EventCard DaringManeuver
daringManeuver = event DaringManeuver Cards.daringManeuver

instance RunMessage env DaringManeuver where
  runMessage msg e@(DaringManeuver attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> e <$ pushAll
      [ skillTestModifier
        (toSource attrs)
        (InvestigatorTarget iid)
        (AnySkillValue 2)
      , Discard (toTarget attrs)
      ]
    _ -> DaringManeuver <$> runMessage msg attrs
