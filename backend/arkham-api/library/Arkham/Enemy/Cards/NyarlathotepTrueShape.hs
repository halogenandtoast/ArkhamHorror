module Arkham.Enemy.Cards.NyarlathotepTrueShape (
  nyarlathotepTrueShape,
  NyarlathotepTrueShape (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NyarlathotepTrueShape = NyarlathotepTrueShape EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepTrueShape :: EnemyCard NyarlathotepTrueShape
nyarlathotepTrueShape = enemy NyarlathotepTrueShape Cards.nyarlathotepTrueShape

instance RunMessage NyarlathotepTrueShape where
  runMessage msg (NyarlathotepTrueShape attrs) =
    NyarlathotepTrueShape <$> runMessage msg attrs
