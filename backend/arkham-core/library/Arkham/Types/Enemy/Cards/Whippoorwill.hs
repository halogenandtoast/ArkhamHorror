module Arkham.Types.Enemy.Cards.Whippoorwill
  ( Whippoorwill(..)
  , whippoorwill
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Whippoorwill = Whippoorwill EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whippoorwill :: EnemyCard Whippoorwill
whippoorwill = enemy Whippoorwill Cards.whippoorwill
  $ (sanityDamageL .~ 1)
  . (fightL .~ 1)
  . (healthL .~ Static 1)
  . (evadeL .~ 4)

instance HasId LocationId env InvestigatorId => HasModifiersFor env Whippoorwill where
  getModifiersFor _ (InvestigatorTarget iid) (Whippoorwill attrs) = do
    locationId <- getId iid
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | locationId == enemyLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Whippoorwill where
  getActions i window (Whippoorwill attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Whippoorwill where
  runMessage msg (Whippoorwill attrs) = Whippoorwill <$> runMessage msg attrs
