module Arkham.Event.Events.SneakAttack2 (sneakAttack2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype SneakAttack2 = SneakAttack2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sneakAttack2 :: EventCard SneakAttack2
sneakAttack2 = event SneakAttack2 Cards.sneakAttack2

instance RunMessage SneakAttack2 where
  runMessage msg e@(SneakAttack2 attrs) = runQueueT $ case msg of
    PlayThisEvent you (is attrs -> True) -> do
      chooseDamageEnemy you attrs (locationWithInvestigator you) EnemyNotEngagedWithYou 2
      pure e
    _ -> SneakAttack2 <$> liftRunMessage msg attrs
