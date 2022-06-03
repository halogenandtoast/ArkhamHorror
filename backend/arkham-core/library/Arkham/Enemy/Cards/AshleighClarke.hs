module Arkham.Enemy.Cards.AshleighClarke
  ( ashleighClarke
  , AshleighClarke(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Phase
import Arkham.Target

newtype AshleighClarke = AshleighClarke EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ashleighClarke :: EnemyCard AshleighClarke
ashleighClarke =
  enemy AshleighClarke Cards.ashleighClarke (2, Static 5, 4) (0, 2)

instance (HasId LocationId env InvestigatorId, HasPhase env) => HasModifiersFor env AshleighClarke where
  getModifiersFor _ (InvestigatorTarget iid) (AshleighClarke attrs) = do
    lid <- getId iid
    phase <- getPhase
    pure $ toModifiers
      attrs
      [ CannotDrawCards | phase == UpkeepPhase && Just lid == enemyLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage AshleighClarke where
  runMessage msg (AshleighClarke attrs) =
    AshleighClarke <$> runMessage msg attrs
