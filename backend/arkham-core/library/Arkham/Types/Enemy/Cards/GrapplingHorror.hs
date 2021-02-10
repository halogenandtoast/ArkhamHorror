module Arkham.Types.Enemy.Cards.GrapplingHorror
  ( GrapplingHorror(..)
  , grapplingHorror
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype GrapplingHorror = GrapplingHorror EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grapplingHorror :: EnemyId -> GrapplingHorror
grapplingHorror uuid =
  GrapplingHorror
    $ baseAttrs uuid "02182"
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
