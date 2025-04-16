module Arkham.Enemy.Cards.Poltergeist (poltergeist) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Trait

newtype Poltergeist = Poltergeist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poltergeist :: EnemyCard Poltergeist
poltergeist = enemy Poltergeist Cards.poltergeist (3, Static 2, 4) (0, 2)

instance HasAbilities Poltergeist where
  getAbilities (Poltergeist a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance HasModifiersFor Poltergeist where
  getModifiersFor (Poltergeist a) = do
    modifySelf
      a
      [CannotBeDamagedByPlayerSourcesExcept $ SourceMatchesAny $ map SourceWithTrait [Spell, Relic]]

instance RunMessage Poltergeist where
  runMessage msg e@(Poltergeist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 attrs
      pure e
    _ -> Poltergeist <$> liftRunMessage msg attrs
