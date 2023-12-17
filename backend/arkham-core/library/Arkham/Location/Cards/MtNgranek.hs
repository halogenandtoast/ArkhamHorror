module Arkham.Location.Cards.MtNgranek
  ( mtNgranek
  , MtNgranek(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MtNgranek = MtNgranek LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mtNgranek :: LocationCard MtNgranek
mtNgranek = location MtNgranek Cards.mtNgranek 3 (PerPlayer 1)

instance HasAbilities MtNgranek where
  getAbilities (MtNgranek attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage MtNgranek where
  runMessage msg (MtNgranek attrs) =
    MtNgranek <$> runMessage msg attrs
