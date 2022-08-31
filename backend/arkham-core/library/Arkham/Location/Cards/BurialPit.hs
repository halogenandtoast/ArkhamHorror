module Arkham.Location.Cards.BurialPit
  ( burialPit
  , BurialPit(..)
  ) where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BurialPit = BurialPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialPit :: LocationCard BurialPit
burialPit = locationWith
  BurialPit
  Cards.burialPit
  3
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities BurialPit where
  getAbilities (BurialPit attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BurialPit where
  runMessage msg (BurialPit attrs) = BurialPit <$> runMessage msg attrs
