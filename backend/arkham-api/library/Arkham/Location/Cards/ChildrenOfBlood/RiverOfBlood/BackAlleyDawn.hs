module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.BackAlleyDawn (backAlleyDawn) where

import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted

newtype BackAlleyDawn = BackAlleyDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backAlleyDawn :: LocationCard BackAlleyDawn
backAlleyDawn = symbolLabel $ location BackAlleyDawn Cards.backAlleyDawn 3 (PerPlayer 2)

instance HasAbilities BackAlleyDawn where
  getAbilities (BackAlleyDawn a) =
    extendRevealed a []

instance RunMessage BackAlleyDawn where
  runMessage msg (BackAlleyDawn attrs) = runQueueT $ case msg of
    _ -> BackAlleyDawn <$> liftRunMessage msg attrs
