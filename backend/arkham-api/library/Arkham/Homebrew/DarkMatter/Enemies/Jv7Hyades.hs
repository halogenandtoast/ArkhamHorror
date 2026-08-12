module Arkham.Homebrew.DarkMatter.Enemies.Jv7Hyades (jv7Hyades) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers
import Arkham.LocationSymbol qualified as LS
import Arkham.Message.Lifted.Choose

newtype Jv7Hyades = Jv7Hyades EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jv7Hyades :: EnemyCard Jv7Hyades
jv7Hyades = enemy Jv7Hyades Cards.jv7Hyades

instance HasAbilities Jv7Hyades where
  getAbilities (Jv7Hyades a) = extend1 a $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage Jv7Hyades where
  runMessage msg e@(Jv7Hyades attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid do
        connectionLabeled' LS.Trefoil $ scan iid (attrs.ability 1) [LS.Trefoil]
        connectionLabeled' LS.T $ scan iid (attrs.ability 1) [LS.T]
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Jv7Hyades <$> liftRunMessage msg attrs
