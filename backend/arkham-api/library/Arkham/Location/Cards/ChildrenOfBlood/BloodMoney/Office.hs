module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Office (office) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype Office = Office LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = symbolLabel $ location Office Cards.office 4 (PerPlayer 1)

instance HasAbilities Office where
  getAbilities (Office a) = extendRevealed a []

instance RunMessage Office where
  runMessage msg (Office attrs) = runQueueT $ Office <$> liftRunMessage msg attrs
