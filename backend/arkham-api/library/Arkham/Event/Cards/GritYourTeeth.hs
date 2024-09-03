module Arkham.Event.Cards.GritYourTeeth (
  gritYourTeeth,
  GritYourTeeth (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner

newtype GritYourTeeth = GritYourTeeth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gritYourTeeth :: EventCard GritYourTeeth
gritYourTeeth = event GritYourTeeth Cards.gritYourTeeth

instance RunMessage GritYourTeeth where
  runMessage msg e@(GritYourTeeth attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e
        <$ pushAll
          [ CreateWindowModifierEffect
              EffectRoundWindow
              (EffectModifiers $ toModifiers attrs [AnySkillValue 1])
              (toSource attrs)
              (InvestigatorTarget iid)
          ]
    _ -> GritYourTeeth <$> runMessage msg attrs
