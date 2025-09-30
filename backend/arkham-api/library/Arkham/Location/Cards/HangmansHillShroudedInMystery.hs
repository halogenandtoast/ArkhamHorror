module Arkham.Location.Cards.HangmansHillShroudedInMystery (hangmansHillShroudedInMystery) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype HangmansHillShroudedInMystery = HangmansHillShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansHillShroudedInMystery :: LocationCard HangmansHillShroudedInMystery
hangmansHillShroudedInMystery = location HangmansHillShroudedInMystery Cards.hangmansHillShroudedInMystery 4 (PerPlayer 1)

instance HasAbilities HangmansHillShroudedInMystery where
  getAbilities (HangmansHillShroudedInMystery a) =
    extendRevealed1 a $ fastAbility a 1 (DamageCost (a.ability 1) YouTarget 1) Here

instance RunMessage HangmansHillShroudedInMystery where
  runMessage msg l@(HangmansHillShroudedInMystery attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      removeBreaches attrs 1
      placeBreaches act 1
      pure l
    _ -> HangmansHillShroudedInMystery <$> liftRunMessage msg attrs
