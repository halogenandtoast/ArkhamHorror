module Arkham.Enemy.Cards.ShadowHound (shadowHound, ShadowHound (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ShadowHound = ShadowHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowHound :: EnemyCard ShadowHound
shadowHound =
  enemyWith ShadowHound Cards.shadowHound (2, Static 3, 1) (1, 0)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities ShadowHound where
  getAbilities (ShadowHound a) =
    extend a [mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)]

instance RunMessage ShadowHound where
  runMessage msg e@(ShadowHound attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runHauntedAbilities iid
      pure e
    _ -> ShadowHound <$> runMessage msg attrs
