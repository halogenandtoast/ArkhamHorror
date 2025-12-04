module Arkham.Location.Cards.MelatisShop (melatisShop) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MelatisShop = MelatisShop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

melatisShop :: LocationCard MelatisShop
melatisShop = symbolLabel $ location MelatisShop Cards.melatisShop 0 (Static 0)

instance HasAbilities MelatisShop where
  getAbilities (MelatisShop a) =
    extendRevealed a []

instance RunMessage MelatisShop where
  runMessage msg (MelatisShop attrs) = runQueueT $ case msg of
    _ -> MelatisShop <$> liftRunMessage msg attrs
