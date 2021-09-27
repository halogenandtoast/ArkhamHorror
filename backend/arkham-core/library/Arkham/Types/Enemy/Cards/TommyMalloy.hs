module Arkham.Types.Enemy.Cards.TommyMalloy
  ( tommyMalloy
  , TommyMalloy(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey

newtype TommyMalloy = TommyMalloy EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tommyMalloy :: EnemyCard TommyMalloy
tommyMalloy = enemyWith
  TommyMalloy
  Cards.tommyMalloy
  (2, Static 3, 3)
  (2, 0)
  (preyL .~ SetToBearer)

instance EnemyRunner env => RunMessage env TommyMalloy where
  runMessage msg (TommyMalloy attrs) = TommyMalloy <$> runMessage msg attrs
