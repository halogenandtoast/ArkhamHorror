module Arkham.Location.Cards.WalterGilmansRoom
  ( walterGilmansRoom
  , WalterGilmansRoom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype WalterGilmansRoom = WalterGilmansRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walterGilmansRoom :: LocationCard WalterGilmansRoom
walterGilmansRoom =
  location WalterGilmansRoom Cards.walterGilmansRoom 4 (PerPlayer 1)

instance HasAbilities WalterGilmansRoom where
  getAbilities (WalterGilmansRoom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage WalterGilmansRoom where
  runMessage msg (WalterGilmansRoom attrs) =
    WalterGilmansRoom <$> runMessage msg attrs
