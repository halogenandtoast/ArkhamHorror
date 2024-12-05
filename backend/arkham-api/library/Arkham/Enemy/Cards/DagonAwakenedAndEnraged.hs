module Arkham.Enemy.Cards.DagonAwakenedAndEnraged (
  dagonAwakenedAndEnraged,
  DagonAwakenedAndEnraged (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype DagonAwakenedAndEnraged = DagonAwakenedAndEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dagonAwakenedAndEnraged :: EnemyCard DagonAwakenedAndEnraged
dagonAwakenedAndEnraged = enemy DagonAwakenedAndEnraged Cards.dagonAwakenedAndEnraged (4, Static 6, 4) (2, 3)

instance HasModifiersFor DagonAwakenedAndEnraged where
  getModifiersFor (DagonAwakenedAndEnraged a) = do
    healthModifier <- perPlayer 6
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities DagonAwakenedAndEnraged where
  getAbilities (DagonAwakenedAndEnraged a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ forced
      $ SkillTestResult #after You AnySkillTest #failure

instance RunMessage DagonAwakenedAndEnraged where
  runMessage msg e@(DagonAwakenedAndEnraged attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      when (attrs.exhausted) $ readyThis attrs
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> DagonAwakenedAndEnraged <$> liftRunMessage msg attrs
