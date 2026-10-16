{- | "__Revelation__ - Test {willpower} (X), where X is your location's shroud
value. If you fail, take 2 horror."
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.HauntingNoises (hauntingNoises) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Treachery.Import.Lifted

newtype HauntingNoises = HauntingNoises TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntingNoises :: TreacheryCard HauntingNoises
hauntingNoises = treachery HauntingNoises Cards.hauntingNoises

instance RunMessage HauntingNoises where
  runMessage msg t@(HauntingNoises attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> HauntingNoises <$> liftRunMessage msg attrs
