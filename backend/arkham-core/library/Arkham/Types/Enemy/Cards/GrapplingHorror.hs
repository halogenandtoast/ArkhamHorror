module Arkham.Types.Enemy.Cards.GrapplingHorror
  ( GrapplingHorror(..)
  , grapplingHorror
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype GrapplingHorror = GrapplingHorror EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grapplingHorror :: EnemyCard GrapplingHorror
grapplingHorror = enemy GrapplingHorror Cards.grapplingHorror
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 3)
  . (healthL .~ Static 3)
  . (evadeL .~ 2)

instance HasModifiersFor env GrapplingHorror where
  getModifiersFor _ (InvestigatorTarget iid) (GrapplingHorror a@EnemyAttrs {..}) =
    if iid `elem` enemyEngagedInvestigators
      then pure $ toModifiers a [CannotMove]
      else pure []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env GrapplingHorror where
  getActions i window (GrapplingHorror attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GrapplingHorror where
  runMessage msg (GrapplingHorror attrs) =
    GrapplingHorror <$> runMessage msg attrs
