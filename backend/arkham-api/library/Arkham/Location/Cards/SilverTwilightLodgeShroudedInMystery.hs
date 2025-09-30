module Arkham.Location.Cards.SilverTwilightLodgeShroudedInMystery (
  silverTwilightLodgeShroudedInMystery,
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype SilverTwilightLodgeShroudedInMystery = SilverTwilightLodgeShroudedInMystery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeShroudedInMystery :: LocationCard SilverTwilightLodgeShroudedInMystery
silverTwilightLodgeShroudedInMystery =
  location
    SilverTwilightLodgeShroudedInMystery
    Cards.silverTwilightLodgeShroudedInMystery
    4
    (PerPlayer 1)

instance HasAbilities SilverTwilightLodgeShroudedInMystery where
  getAbilities (SilverTwilightLodgeShroudedInMystery a) =
    extendRevealed1 a $ fastAbility a 1 (HorrorCost (a.ability 1) YouTarget 1) Here

instance RunMessage SilverTwilightLodgeShroudedInMystery where
  runMessage msg l@(SilverTwilightLodgeShroudedInMystery attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      removeBreaches attrs 1
      placeBreaches act 1
      pure l
    _ -> SilverTwilightLodgeShroudedInMystery <$> liftRunMessage msg attrs
