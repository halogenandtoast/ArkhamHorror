module Arkham.Enemy.Cards.AgentOfTheKing (agentOfTheKing) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Matcher

newtype AgentOfTheKing = AgentOfTheKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentOfTheKing :: EnemyCard AgentOfTheKing
agentOfTheKing =
  enemy AgentOfTheKing Cards.agentOfTheKing (4, Static 4, 2) (1, 2)
    & setPrey MostClues

instance HasAbilities AgentOfTheKing where
  getAbilities (AgentOfTheKing a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyAttacks #after (You <> InvestigatorWithAnyClues) AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ EnemyDefeated #when You ByAny $ be a <> EnemyWithAnyClues
      ]

instance RunMessage AgentOfTheKing where
  runMessage msg e@(AgentOfTheKing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spendClues iid 1
      placeClues (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeClues (attrs.ability 2) attrs (enemyClues attrs)
      gainClues iid (attrs.ability 2) (enemyClues attrs)
      pure e
    _ -> AgentOfTheKing <$> liftRunMessage msg attrs
