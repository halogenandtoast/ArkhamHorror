module Arkham.Enemy.Cards.Zealot (zealot) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype Zealot = Zealot EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zealot :: EnemyCard Zealot
zealot = enemy Zealot Cards.zealot (2, Static 1, 2) (1, 0)

instance HasAbilities Zealot where
  getAbilities (Zealot a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage Zealot where
  runMessage msg e@(Zealot attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    _ -> Zealot <$> liftRunMessage msg attrs
