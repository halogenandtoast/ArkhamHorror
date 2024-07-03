module Arkham.Enemy.Cards.AgentFletcher (agentFletcher, AgentFletcher (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher

newtype AgentFletcher = AgentFletcher EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor AgentFletcher where
  getModifiersFor (InvestigatorTarget iid) (AgentFletcher a) = do
    maybeModified a do
      guardM $ lift $ iid <=~> investigatorIs Investigators.kymaniJones
      st <- MaybeT getSkillTest
      guard $ isTarget a st.target
      guard $ #evade `elem` st.action
      pure [SetSkillValue #intellect 0]
  getModifiersFor _ _ = pure []

agentFletcher :: EnemyCard AgentFletcher
agentFletcher =
  enemyWith
    AgentFletcher
    Cards.agentFletcher
    (2, Static 3, 3)
    (1, 1)
    (\a -> a & preyL .~ BearerOf (toId a))

instance RunMessage AgentFletcher where
  runMessage msg (AgentFletcher attrs) =
    AgentFletcher <$> runMessage msg attrs
