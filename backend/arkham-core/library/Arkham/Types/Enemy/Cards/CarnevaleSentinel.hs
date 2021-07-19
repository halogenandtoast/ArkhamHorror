module Arkham.Types.Enemy.Cards.CarnevaleSentinel
  ( carnevaleSentinel
  , CarnevaleSentinel(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype CarnevaleSentinel = CarnevaleSentinel EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

carnevaleSentinel :: EnemyCard CarnevaleSentinel
carnevaleSentinel =
  enemy CarnevaleSentinel Cards.carnevaleSentinel (3, Static 3, 3) (2, 0)

instance HasModifiersFor env CarnevaleSentinel where
  getModifiersFor _ (AssetTarget aid) (CarnevaleSentinel attrs) = do
    mlid <- getId @(Maybe LocationId) aid
    case mlid of
      Just lid | lid == enemyLocation attrs -> undefined
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env CarnevaleSentinel where
  getActions i window (CarnevaleSentinel attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env CarnevaleSentinel where
  runMessage msg (CarnevaleSentinel attrs) =
    CarnevaleSentinel <$> runMessage msg attrs
