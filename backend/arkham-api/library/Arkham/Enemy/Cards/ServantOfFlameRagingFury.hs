module Arkham.Enemy.Cards.ServantOfFlameRagingFury (servantOfFlameRagingFury) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ServantOfFlameRagingFury = ServantOfFlameRagingFury EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

servantOfFlameRagingFury :: EnemyCard ServantOfFlameRagingFury
servantOfFlameRagingFury =
  enemy ServantOfFlameRagingFury Cards.servantOfFlameRagingFury (4, PerPlayer 5, 4) (2, 2)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance RunMessage ServantOfFlameRagingFury where
  runMessage msg (ServantOfFlameRagingFury attrs) = ServantOfFlameRagingFury <$> runMessage msg attrs
