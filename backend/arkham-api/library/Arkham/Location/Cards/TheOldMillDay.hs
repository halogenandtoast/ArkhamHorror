module Arkham.Location.Cards.TheOldMillDay (theOldMillDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheOldMillDay = TheOldMillDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillDay :: LocationCard TheOldMillDay
theOldMillDay = symbolLabel $ location TheOldMillDay Cards.theOldMillDay 2 (Static 1)

instance HasAbilities TheOldMillDay where
  getAbilities (TheOldMillDay a) =
    extendRevealed1 a $ restricted a 1 Here $ freeReaction (DiscoverClues #after You (be a) (atLeast 1))

instance RunMessage TheOldMillDay where
  runMessage msg l@(TheOldMillDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 12
      pure l
    _ -> TheOldMillDay <$> liftRunMessage msg attrs
