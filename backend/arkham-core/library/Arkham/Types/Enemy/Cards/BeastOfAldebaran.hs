module Arkham.Types.Enemy.Cards.BeastOfAldebaran
  ( beastOfAldebaran
  , BeastOfAldebaran(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype BeastOfAldebaran = BeastOfAldebaran EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beastOfAldebaran :: EnemyCard BeastOfAldebaran
beastOfAldebaran =
  enemy BeastOfAldebaran Cards.beastOfAldebaran (3, Static 7, 5) (2, 1)

instance EnemyRunner env => RunMessage env BeastOfAldebaran where
  runMessage msg (BeastOfAldebaran attrs) =
    BeastOfAldebaran <$> runMessage msg attrs
