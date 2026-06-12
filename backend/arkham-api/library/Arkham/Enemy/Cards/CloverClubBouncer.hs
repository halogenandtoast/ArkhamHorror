module Arkham.Enemy.Cards.CloverClubBouncer (cloverClubBouncer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier

newtype CloverClubBouncer = CloverClubBouncer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBouncer :: EnemyCard CloverClubBouncer
cloverClubBouncer = enemy CloverClubBouncer Cards.cloverClubBouncer (1, Static 3, 1) (2, 0)

instance HasModifiersFor CloverClubBouncer where
  getModifiersFor (CloverClubBouncer a) = do
    n <- getMax0 <$> selectAgg Max0 InvestigatorResources (investigatorEngagedWith a.id)
    let bonus = n `div` 5
    modifySelfWhen a (bonus > 0) [Modifier.EnemyFight bonus, Modifier.EnemyEvade bonus]

instance HasAbilities CloverClubBouncer where
  getAbilities (CloverClubBouncer a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 3)

instance RunMessage CloverClubBouncer where
  runMessage msg e@(CloverClubBouncer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      disengageEnemy iid attrs
      exhaustThis attrs
      nextPhaseModifier #upkeep (attrs.ability 1) attrs Modifier.DoesNotReadyDuringUpkeep
      pure e
    _ -> CloverClubBouncer <$> liftRunMessage msg attrs
