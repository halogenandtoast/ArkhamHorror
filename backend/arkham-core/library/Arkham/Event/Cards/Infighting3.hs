module Arkham.Event.Cards.Infighting3 (
  infighting3,
  Infighting3 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Infighting3 = Infighting3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

infighting3 :: EventCard Infighting3
infighting3 = event Infighting3 Cards.infighting3

instance RunMessage Infighting3 where
  runMessage msg e@(Infighting3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ phaseModifier attrs iid (CancelAttacksByEnemies (toCard attrs) NonEliteEnemy)
      pure e
    _ -> Infighting3 <$> runMessage msg attrs
