module Arkham.Location.Cards.TowersOfPnakotus
  ( towersOfPnakotus
  , TowersOfPnakotus(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TowersOfPnakotus = TowersOfPnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towersOfPnakotus :: LocationCard TowersOfPnakotus
towersOfPnakotus =
  location TowersOfPnakotus Cards.towersOfPnakotus 2 (PerPlayer 2)

instance HasAbilities TowersOfPnakotus where
  getAbilities (TowersOfPnakotus attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TowersOfPnakotus where
  runMessage msg (TowersOfPnakotus attrs) =
    TowersOfPnakotus <$> runMessage msg attrs
