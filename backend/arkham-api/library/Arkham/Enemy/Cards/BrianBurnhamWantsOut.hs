module Arkham.Enemy.Cards.BrianBurnhamWantsOut
  ( brianBurnhamWantsOut
  , BrianBurnhamWantsOut(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype BrianBurnhamWantsOut = BrianBurnhamWantsOut EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

brianBurnhamWantsOut :: EnemyCard BrianBurnhamWantsOut
brianBurnhamWantsOut = enemy BrianBurnhamWantsOut Cards.brianBurnhamWantsOut (3, Static 3, 5) (1, 0)

instance RunMessage BrianBurnhamWantsOut where
  runMessage msg (BrianBurnhamWantsOut attrs) =
    BrianBurnhamWantsOut <$> runMessage msg attrs
