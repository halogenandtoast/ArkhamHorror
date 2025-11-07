module Arkham.Location.Cards.GranTeatroDeLaHabana (granTeatroDeLaHabana) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GranTeatroDeLaHabana = GranTeatroDeLaHabana LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

granTeatroDeLaHabana :: LocationCard GranTeatroDeLaHabana
granTeatroDeLaHabana = symbolLabel $ location GranTeatroDeLaHabana Cards.granTeatroDeLaHabana 4 (PerPlayer 1)

instance HasAbilities GranTeatroDeLaHabana where
  getAbilities (GranTeatroDeLaHabana attrs) =
    extendRevealed attrs []

instance RunMessage GranTeatroDeLaHabana where
  runMessage msg (GranTeatroDeLaHabana attrs) = runQueueT $ case msg of
    _ -> GranTeatroDeLaHabana <$> liftRunMessage msg attrs
