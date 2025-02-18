module Arkham.Event.Events.MonsterSlayer (monsterSlayer) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype MonsterSlayer = MonsterSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monsterSlayer :: EventCard MonsterSlayer
monsterSlayer = event MonsterSlayer Cards.monsterSlayer

instance RunMessage MonsterSlayer where
  runMessage msg e@(MonsterSlayer attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DamageDealt 1)
      chooseFightEnemy sid iid attrs
      pure e
    _ -> MonsterSlayer <$> liftRunMessage msg attrs
