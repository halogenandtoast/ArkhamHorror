module Arkham.Enemy.Cards.Whippoorwill
  ( Whippoorwill(..)
  , whippoorwill
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.Target

newtype Whippoorwill = Whippoorwill EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whippoorwill :: EnemyCard Whippoorwill
whippoorwill = enemy Whippoorwill Cards.whippoorwill (2, Static 1, 4) (0, 1)

instance HasId LocationId env InvestigatorId => HasModifiersFor env Whippoorwill where
  getModifiersFor _ (InvestigatorTarget iid) (Whippoorwill attrs) = do
    locationId <- getId iid
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | Just locationId == enemyLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env Whippoorwill where
  runMessage msg (Whippoorwill attrs) = Whippoorwill <$> runMessage msg attrs
