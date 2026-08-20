module Arkham.Enemy.Cards.TheDunwichLegacy.WhereDoomAwaits.SethBishop (sethBishop) where

import Arkham.Enemy.CardDefs.TheDunwichLegacy.WhereDoomAwaits qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SethBishop = SethBishop EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sethBishop :: EnemyCard SethBishop
sethBishop = enemy SethBishop Cards.sethBishop

instance RunMessage SethBishop where
  runMessage msg (SethBishop attrs) = SethBishop <$> runMessage msg attrs
