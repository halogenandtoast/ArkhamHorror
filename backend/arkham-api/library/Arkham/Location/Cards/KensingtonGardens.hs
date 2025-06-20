module Arkham.Location.Cards.KensingtonGardens (kensingtonGardens, KensingtonGardens(..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.RiddlesAndRain.Helpers (scenarioI18n)

newtype KensingtonGardens = KensingtonGardens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kensingtonGardens :: LocationCard KensingtonGardens
kensingtonGardens = location KensingtonGardens Cards.kensingtonGardens 2 (PerPlayer 1)

instance HasAbilities KensingtonGardens where
  getAbilities (KensingtonGardens a) =
    extendRevealed
      a
      [ -- TODO: Expose enemy/decoy abilities not yet implemented
      ]

instance RunMessage KensingtonGardens where
  runMessage msg (KensingtonGardens attrs) =
    KensingtonGardens <$> liftRunMessage msg attrs
