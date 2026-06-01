module Arkham.Location.Cards.TheOldMillEvening (theOldMillEvening) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheOldMillEvening = TheOldMillEvening LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillEvening :: LocationCard TheOldMillEvening
theOldMillEvening = symbolLabel $ location TheOldMillEvening Cards.theOldMillEvening 4 (Static 1)

instance HasAbilities TheOldMillEvening where
  getAbilities (TheOldMillEvening a) =
    extendRevealed1 a $ restricted a 1 Here $ FastAbility Free

instance RunMessage TheOldMillEvening where
  runMessage msg l@(TheOldMillEvening attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 12
      pure l
    _ -> TheOldMillEvening <$> liftRunMessage msg attrs
