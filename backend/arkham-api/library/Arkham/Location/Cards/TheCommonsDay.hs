module Arkham.Location.Cards.TheCommonsDay (theCommonsDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCommonsDay = TheCommonsDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCommonsDay :: LocationCard TheCommonsDay
theCommonsDay = symbolLabel $ location TheCommonsDay Cards.theCommonsDay 1 (Static 0)

instance HasAbilities TheCommonsDay where
  getAbilities (TheCommonsDay a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here actionAbility

instance RunMessage TheCommonsDay where
  runMessage msg l@(TheCommonsDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 16
      pure l
    _ -> TheCommonsDay <$> liftRunMessage msg attrs
