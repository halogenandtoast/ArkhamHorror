module Arkham.Enemy.Cards.AshleighClarke (ashleighClarke, AshleighClarke (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude

newtype AshleighClarke = AshleighClarke EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ashleighClarke :: EnemyCard AshleighClarke
ashleighClarke = enemy AshleighClarke Cards.ashleighClarke (2, Static 5, 4) (0, 2)

instance HasModifiersFor AshleighClarke where
  getModifiersFor (AshleighClarke a) = do
    phase <- getPhase
    modifySelectWhen a (phase == UpkeepPhase) (InvestigatorAt $ locationWithEnemy a) [CannotDrawCards]

instance RunMessage AshleighClarke where
  runMessage msg (AshleighClarke attrs) = AshleighClarke <$> runMessage msg attrs
