module Arkham.Enemy.Cards.SalvatoreNeri (salvatoreNeri, SalvatoreNeri (..)) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner hiding (EnemyEvade)
import Arkham.Helpers.Investigator
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude
import Control.Monad.Fail

newtype SalvatoreNeri = SalvatoreNeri EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

salvatoreNeri :: EnemyCard SalvatoreNeri
salvatoreNeri = enemy SalvatoreNeri Cards.salvatoreNeri (0, Static 3, 0) (0, 2)

instance HasModifiersFor SalvatoreNeri where
  getModifiersFor (SalvatoreNeri attrs) = maybeModifySelf attrs do
    iid <- MaybeT getSkillTestInvestigator
    MaybeT getSkillTestAction >>= \case
      Action.Evade -> do
        evadeValue <- baseSkillValueFor #agility (Just #evade) iid
        pure [Modifier.EnemyEvade evadeValue]
      Action.Fight -> do
        fightValue <- baseSkillValueFor #combat (Just #fight) iid
        pure [Modifier.EnemyFight fightValue]
      _ -> fail "Not valid action type"

instance RunMessage SalvatoreNeri where
  runMessage msg (SalvatoreNeri attrs) = SalvatoreNeri <$> runMessage msg attrs
