module Arkham.Enemy.Cards.Farid (farid) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype Farid = Farid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farid :: EnemyCard Farid
farid =
  enemyWith Farid Cards.farid (3, Static 4, 2) (1, 0)
    $ spawnAtL
    ?~ SpawnAt "Cairo Bazaar"

instance HasAbilities Farid where
  getAbilities (Farid a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage Farid where
  runMessage msg e@(Farid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resources <- field InvestigatorResources iid
      chooseAmounts iid "Amount of resources to spend" (MaxAmountTarget 5) [("Resources", (0, min 5 resources))] attrs
      pure e
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      sid <- getRandom
      when (n > 0) $ push $ SpendResources iid n
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed $ max 0 (7 - n))
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      addToVictory iid attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> Farid <$> liftRunMessage msg attrs
