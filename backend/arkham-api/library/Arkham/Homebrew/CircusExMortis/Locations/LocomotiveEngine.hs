module Arkham.Homebrew.CircusExMortis.Locations.LocomotiveEngine (locomotiveEngine) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype LocomotiveEngine = LocomotiveEngine LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

locomotiveEngine :: LocationCard LocomotiveEngine
locomotiveEngine = location LocomotiveEngine Cards.locomotiveEngine 3 (Static 1)

instance HasAbilities LocomotiveEngine where
  getAbilities (LocomotiveEngine a) =
    extendRevealed1 a $ groupLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage LocomotiveEngine where
  runMessage msg l@(LocomotiveEngine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ LookAtTopOfDeck iid EncounterDeckTarget 4
      pure l
    _ -> LocomotiveEngine <$> liftRunMessage msg attrs
