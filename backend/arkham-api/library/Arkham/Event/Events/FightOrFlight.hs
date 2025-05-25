module Arkham.Event.Events.FightOrFlight (fightOrFlight) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype FightOrFlight = FightOrFlight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fightOrFlight :: EventCard FightOrFlight
fightOrFlight = event FightOrFlight Cards.fightOrFlight

instance RunMessage FightOrFlight where
  runMessage msg e@(FightOrFlight attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      roundModifiers
        attrs
        iid
        [ CalculatedSkillModifier #combat (InvestigatorFieldCalculation iid #horror)
        , CalculatedSkillModifier #agility (InvestigatorFieldCalculation iid #horror)
        ]
      pure e
    _ -> FightOrFlight <$> liftRunMessage msg attrs
