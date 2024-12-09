module Arkham.Enemy.Cards.AsylumGorger (asylumGorger, AsylumGorger (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype AsylumGorger = AsylumGorger EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor AsylumGorger where
  getModifiersFor (AsylumGorger a) =
    modifySelf a $ CannotMakeAttacksOfOpportunity : [CannotAttack | enemyMovedFromHunterKeyword a]

asylumGorger :: EnemyCard AsylumGorger
asylumGorger =
  enemyWith AsylumGorger Cards.asylumGorger (4, Static 5, 4) (3, 3)
    $ spawnAtL
    ?~ SpawnAt "Basement Hall"

instance RunMessage AsylumGorger where
  runMessage msg (AsylumGorger attrs) = AsylumGorger <$> runMessage msg attrs
