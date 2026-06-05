module Arkham.Location.Cards.OozyLakebed (oozyLakebed) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype OozyLakebed = OozyLakebed LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozyLakebed :: LocationCard OozyLakebed
oozyLakebed = locationWith OozyLakebed Cards.oozyLakebed 2 (Static 0) connectsToAdjacent

instance RunMessage OozyLakebed where
  runMessage msg (OozyLakebed attrs) = OozyLakebed <$> runMessage msg attrs
