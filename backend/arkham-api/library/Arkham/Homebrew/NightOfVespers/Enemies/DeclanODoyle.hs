-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Enemies.DeclanODoyle (declanODoyle) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Cards

newtype DeclanODoyle = DeclanODoyle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

declanODoyle :: EnemyCard DeclanODoyle
declanODoyle = enemy DeclanODoyle Cards.declanODoyle

instance RunMessage DeclanODoyle where
  runMessage msg (DeclanODoyle attrs) = DeclanODoyle <$> runMessage msg attrs
