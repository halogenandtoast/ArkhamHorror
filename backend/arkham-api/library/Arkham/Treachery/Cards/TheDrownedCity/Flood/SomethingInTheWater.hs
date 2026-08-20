module Arkham.Treachery.Cards.TheDrownedCity.Flood.SomethingInTheWater (somethingInTheWater) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.TheDrownedCity.Flood qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SomethingInTheWater = SomethingInTheWater TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheWater :: TreacheryCard SomethingInTheWater
somethingInTheWater = treachery SomethingInTheWater Cards.somethingInTheWater

instance RunMessage SomethingInTheWater where
  runMessage msg t@(SomethingInTheWater attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ NearestLocationTo iid CanHaveFloodLevelIncreased
      chooseOrRunOneM iid $ targets locations $ push . IncreaseFloodLevel
      pure t
    _ -> SomethingInTheWater <$> liftRunMessage msg attrs
