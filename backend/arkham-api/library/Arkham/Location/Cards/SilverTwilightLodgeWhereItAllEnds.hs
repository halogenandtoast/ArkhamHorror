module Arkham.Location.Cards.SilverTwilightLodgeWhereItAllEnds (silverTwilightLodgeWhereItAllEnds) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (SilverTwilight))

newtype SilverTwilightLodgeWhereItAllEnds = SilverTwilightLodgeWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightLodgeWhereItAllEnds :: LocationCard SilverTwilightLodgeWhereItAllEnds
silverTwilightLodgeWhereItAllEnds = location SilverTwilightLodgeWhereItAllEnds Cards.silverTwilightLodgeWhereItAllEnds 2 (Static 0)

instance HasAbilities SilverTwilightLodgeWhereItAllEnds where
  getAbilities (SilverTwilightLodgeWhereItAllEnds a) = extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage SilverTwilightLodgeWhereItAllEnds where
  runMessage msg l@(SilverTwilightLodgeWhereItAllEnds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let breaches = countLocationBreaches attrs
      act <- selectJust AnyAct
      findAndDrawEncounterCard iid (#enemy <> CardWithTrait SilverTwilight)
      when (breaches > 0) do
        removeBreaches attrs breaches
        placeBreaches act breaches
      pure l
    _ -> SilverTwilightLodgeWhereItAllEnds <$> liftRunMessage msg attrs
