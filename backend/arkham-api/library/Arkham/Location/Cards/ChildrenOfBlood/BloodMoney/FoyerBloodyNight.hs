module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.FoyerBloodyNight (foyerBloodyNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype FoyerBloodyNight = FoyerBloodyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foyerBloodyNight :: LocationCard FoyerBloodyNight
foyerBloodyNight = symbolLabel $ location FoyerBloodyNight Cards.foyerBloodyNight 2 (Static 0)

instance HasAbilities FoyerBloodyNight where
  getAbilities (FoyerBloodyNight a) = extendRevealed a []

instance RunMessage FoyerBloodyNight where
  runMessage msg (FoyerBloodyNight attrs) = runQueueT $ FoyerBloodyNight <$> liftRunMessage msg attrs
