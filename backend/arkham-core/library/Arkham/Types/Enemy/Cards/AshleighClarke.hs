module Arkham.Types.Enemy.Cards.AshleighClarke
  ( ashleighClarke
  , AshleighClarke(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype AshleighClarke = AshleighClarke EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ashleighClarke :: EnemyCard AshleighClarke
ashleighClarke =
  enemy AshleighClarke Cards.ashleighClarke (0, Static 1, 0) (0, 0)

instance EnemyRunner env => RunMessage env AshleighClarke where
  runMessage msg (AshleighClarke attrs) =
    AshleighClarke <$> runMessage msg attrs
