module Arkham.Types.Enemy.Cards.IshimaruHaruko
  ( ishimaruHaruko
  , IshimaruHaruko(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype IshimaruHaruko = IshimaruHaruko EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ishimaruHaruko :: EnemyCard IshimaruHaruko
ishimaruHaruko =
  enemy IshimaruHaruko Cards.ishimaruHaruko (0, Static 1, 0) (0, 0)

instance EnemyRunner env => RunMessage env IshimaruHaruko where
  runMessage msg (IshimaruHaruko attrs) =
    IshimaruHaruko <$> runMessage msg attrs
