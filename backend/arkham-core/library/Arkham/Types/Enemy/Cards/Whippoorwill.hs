module Arkham.Types.Enemy.Cards.Whippoorwill
  ( Whippoorwill(..)
  , whippoorwill
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Whippoorwill = Whippoorwill EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

whippoorwill :: EnemyCard Whippoorwill
whippoorwill = enemy Whippoorwill Cards.whippoorwill (2, Static 1, 4) (0, 1)

instance HasId LocationId env InvestigatorId => HasModifiersFor env Whippoorwill where
  getModifiersFor _ (InvestigatorTarget iid) (Whippoorwill attrs) = do
    locationId <- getId iid
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | locationId == enemyLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env Whippoorwill where
  runMessage msg (Whippoorwill attrs) = Whippoorwill <$> runMessage msg attrs
