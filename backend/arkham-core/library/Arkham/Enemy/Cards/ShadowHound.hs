module Arkham.Enemy.Cards.ShadowHound
  ( shadowHound
  , ShadowHound(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype ShadowHound = ShadowHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

shadowHound :: EnemyCard ShadowHound
shadowHound = enemy ShadowHound Cards.shadowHound (2, Static 3, 1) (1, 0)

instance RunMessage ShadowHound where
  runMessage msg (ShadowHound attrs) =
    ShadowHound <$> runMessage msg attrs
