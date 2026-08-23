module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Balcony (balcony) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony = symbolLabel $ location Balcony Cards.balcony 1 (Static 0)

instance HasAbilities Balcony where
  getAbilities (Balcony a) = extendRevealed a []

instance RunMessage Balcony where
  runMessage msg (Balcony attrs) = runQueueT $ Balcony <$> liftRunMessage msg attrs
