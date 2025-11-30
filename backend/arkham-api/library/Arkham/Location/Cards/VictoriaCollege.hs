module Arkham.Location.Cards.VictoriaCollege (victoriaCollege) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype VictoriaCollege = VictoriaCollege LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaCollege :: LocationCard VictoriaCollege
victoriaCollege = symbolLabel $ location VictoriaCollege Cards.victoriaCollege 0 (Static 0)

instance HasAbilities VictoriaCollege where
  getAbilities (VictoriaCollege a) =
    extendRevealed a []

instance RunMessage VictoriaCollege where
  runMessage msg (VictoriaCollege attrs) = runQueueT $ case msg of
    _ -> VictoriaCollege <$> liftRunMessage msg attrs
