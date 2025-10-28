module Arkham.Enemy.Cards.ShadowHound (shadowHound) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype ShadowHound = ShadowHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowHound :: EnemyCard ShadowHound
shadowHound =
  enemy ShadowHound Cards.shadowHound (2, Static 3, 1) (1, 0)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities ShadowHound where
  getAbilities (ShadowHound a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage ShadowHound where
  runMessage msg e@(ShadowHound attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runHauntedAbilities iid
      pure e
    _ -> ShadowHound <$> liftRunMessage msg attrs
