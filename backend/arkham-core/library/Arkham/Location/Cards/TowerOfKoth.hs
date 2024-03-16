module Arkham.Location.Cards.TowerOfKoth
  ( towerOfKoth
  , TowerOfKoth(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TowerOfKoth = TowerOfKoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerOfKoth :: LocationCard TowerOfKoth
towerOfKoth = location TowerOfKoth Cards.towerOfKoth 5 (Static 0)

instance HasAbilities TowerOfKoth where
  getAbilities (TowerOfKoth attrs) =
    extendRevealed attrs []

instance RunMessage TowerOfKoth where
  runMessage msg (TowerOfKoth attrs) =
    TowerOfKoth <$> runMessage msg attrs
