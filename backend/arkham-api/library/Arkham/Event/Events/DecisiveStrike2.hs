module Arkham.Event.Events.DecisiveStrike2 (decisiveStrike2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype DecisiveStrike2 = DecisiveStrike2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decisiveStrike2 :: EventCard DecisiveStrike2
decisiveStrike2 = event DecisiveStrike2 Cards.decisiveStrike2

instance RunMessage DecisiveStrike2 where
  runMessage msg e@(DecisiveStrike2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid attrs
      pure e
    EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      gainResources attrs.controller attrs 5
      pure e
    _ -> DecisiveStrike2 <$> liftRunMessage msg attrs
