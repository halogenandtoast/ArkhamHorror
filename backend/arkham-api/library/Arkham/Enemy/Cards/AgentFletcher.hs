module Arkham.Enemy.Cards.AgentFletcher (agentFletcher, AgentFletcher (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMaybe)
import Arkham.Helpers.SkillTest (isEvading)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher

newtype AgentFletcher = AgentFletcher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor AgentFletcher where
  getModifiersFor (AgentFletcher a) =
    modifySelectMaybe a (investigatorIs Investigators.kymaniJones) \_ -> do
      guardM $ isEvading a
      pure [SetSkillValue #intellect 0]

agentFletcher :: EnemyCard AgentFletcher
agentFletcher =
  enemyWith AgentFletcher Cards.agentFletcher (2, Static 3, 3) (1, 1)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance RunMessage AgentFletcher where
  runMessage msg (AgentFletcher attrs) = AgentFletcher <$> runMessage msg attrs
