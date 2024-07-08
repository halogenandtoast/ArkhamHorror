module Arkham.Location.Cards.SouthsideMasBoardingHouse (
  SouthsideMasBoardingHouse (..),
  southsideMasBoardingHouse,
) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (southsideMasBoardingHouse)
import Arkham.Location.Import.Lifted
import Arkham.Strategy

newtype SouthsideMasBoardingHouse = SouthsideMasBoardingHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southsideMasBoardingHouse :: LocationCard SouthsideMasBoardingHouse
southsideMasBoardingHouse = location SouthsideMasBoardingHouse Cards.southsideMasBoardingHouse 2 (PerPlayer 1)

instance HasAbilities SouthsideMasBoardingHouse where
  getAbilities (SouthsideMasBoardingHouse x) =
    extendRevealed x [playerLimit PerGame $ restrictedAbility x 1 Here actionAbility]

instance RunMessage SouthsideMasBoardingHouse where
  runMessage msg l@(SouthsideMasBoardingHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromDeck] #ally $ DrawFound iid 1
      pure l
    _ -> SouthsideMasBoardingHouse <$> liftRunMessage msg attrs
