module Arkham.Location.Cards.TheAtwoodHouseDay (theAtwoodHouseDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheAtwoodHouseDay = TheAtwoodHouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAtwoodHouseDay :: LocationCard TheAtwoodHouseDay
theAtwoodHouseDay = symbolLabel $ location TheAtwoodHouseDay Cards.theAtwoodHouseDay 4 (Static 0)

instance HasAbilities TheAtwoodHouseDay where
  getAbilities (TheAtwoodHouseDay a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage TheAtwoodHouseDay where
  runMessage msg l@(TheAtwoodHouseDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 13
      pure l
    _ -> TheAtwoodHouseDay <$> liftRunMessage msg attrs
