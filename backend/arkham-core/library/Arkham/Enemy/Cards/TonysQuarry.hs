module Arkham.Enemy.Cards.TonysQuarry
  ( tonysQuarry
  , TonysQuarry(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype TonysQuarry = TonysQuarry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tonysQuarry :: EnemyCard TonysQuarry
tonysQuarry = enemy TonysQuarry Cards.tonysQuarry (4, Static 3, 1) (1, 2)

instance RunMessage TonysQuarry where
  runMessage msg (TonysQuarry attrs) =
    TonysQuarry <$> runMessage msg attrs
