module Arkham.Location.Cards.HangmansHillWhereItAllEnds (hangmansHillWhereItAllEnds) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Trait (Trait (Witch))

newtype HangmansHillWhereItAllEnds = HangmansHillWhereItAllEnds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangmansHillWhereItAllEnds :: LocationCard HangmansHillWhereItAllEnds
hangmansHillWhereItAllEnds = location HangmansHillWhereItAllEnds Cards.hangmansHillWhereItAllEnds 2 (Static 0)

instance HasAbilities HangmansHillWhereItAllEnds where
  getAbilities (HangmansHillWhereItAllEnds a) = extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage HangmansHillWhereItAllEnds where
  runMessage msg l@(HangmansHillWhereItAllEnds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let breaches = countLocationBreaches attrs
      act <- selectJust AnyAct
      findAndDrawEncounterCard iid (#enemy <> CardWithTrait Witch)
      when (breaches > 0) do
        removeBreaches attrs breaches
        placeBreaches act breaches
      pure l
    _ -> HangmansHillWhereItAllEnds <$> liftRunMessage msg attrs
