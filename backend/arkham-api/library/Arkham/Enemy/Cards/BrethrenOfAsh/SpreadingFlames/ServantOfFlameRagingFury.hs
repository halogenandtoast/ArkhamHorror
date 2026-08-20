module Arkham.Enemy.Cards.BrethrenOfAsh.SpreadingFlames.ServantOfFlameRagingFury (servantOfFlameRagingFury) where

import Arkham.Enemy.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ServantOfFlameRagingFury = ServantOfFlameRagingFury EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

servantOfFlameRagingFury :: EnemyCard ServantOfFlameRagingFury
servantOfFlameRagingFury =
  enemy ServantOfFlameRagingFury Cards.servantOfFlameRagingFury
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)
