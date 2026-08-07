module Arkham.Location.Cards.RlyehStreets (rlyehStreets) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RlyehStreets = RlyehStreets LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rlyehStreets :: LocationCard RlyehStreets
rlyehStreets = location RlyehStreets Cards.rlyehStreets 2 (PerPlayer  3)

instance HasModifiersFor RlyehStreets where
  getModifiersFor (RlyehStreets a) = modifySelf a [CannotBeMoved, CannotLeavePlay]

instance HasAbilities RlyehStreets where
  getAbilities (RlyehStreets a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ actionAbilityWithCost (AtLeastOne (Fixed 3) (ResourceCost 1))

instance RunMessage RlyehStreets where
  runMessage msg l@(RlyehStreets attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      revealedLocations <- select RevealedLocation
      chooseTargetM iid revealedLocations \lid ->
        placeClues (attrs.ability 1) lid n
      pure l
    _ -> RlyehStreets <$> liftRunMessage msg attrs
