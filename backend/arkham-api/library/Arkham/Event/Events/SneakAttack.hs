module Arkham.Event.Events.SneakAttack where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype SneakAttack = SneakAttack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

instance RunMessage SneakAttack where
  runMessage msg e@(SneakAttack attrs) = runQueueT $ case msg of
    PlayThisEvent you (is attrs -> True) -> do
      enemies <- select $ ExhaustedEnemy <> enemiesColocatedWith you
      chooseTargetM you enemies $ nonAttackEnemyDamage attrs 2
      pure e
    _ -> SneakAttack <$> liftRunMessage msg attrs
