module Arkham.Location.Cards.TempleOfTheFang (templeOfTheFang, TempleOfTheFang (..)) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude

newtype TempleOfTheFang = TempleOfTheFang LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

templeOfTheFang :: LocationCard TempleOfTheFang
templeOfTheFang =
  location TempleOfTheFang Cards.templeOfTheFang 2 (PerPlayer 1)

instance HasModifiersFor TempleOfTheFang where
  getModifiersFor (TempleOfTheFang a) = do
    n <- getVengeanceInVictoryDisplay
    modifySelf a [ShroudModifier n | n > 0]

instance RunMessage TempleOfTheFang where
  runMessage msg (TempleOfTheFang attrs) =
    TempleOfTheFang <$> runMessage msg attrs
