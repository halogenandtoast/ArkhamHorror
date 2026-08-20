module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.BackAlleyDusk (backAlleyDusk) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype BackAlleyDusk = BackAlleyDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backAlleyDusk :: LocationCard BackAlleyDusk
backAlleyDusk = symbolLabel $ location BackAlleyDusk Cards.backAlleyDusk 3 (PerPlayer 2)

instance HasAbilities BackAlleyDusk where
  getAbilities (BackAlleyDusk a) =
    extendRevealed a []

instance RunMessage BackAlleyDusk where
  runMessage msg (BackAlleyDusk attrs) = runQueueT $ case msg of
    _ -> BackAlleyDusk <$> liftRunMessage msg attrs
