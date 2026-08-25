module Arkham.Homebrew.CircusExMortis.Locations.SecludedTent_053 (secludedTent_053) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Phase

newtype SecludedTent_053 = SecludedTent_053 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secludedTent_053 :: LocationCard SecludedTent_053
secludedTent_053 = location SecludedTent_053 Cards.secludedTent_053 3 (Static 2)

instance HasAbilities SecludedTent_053 where
  getAbilities (SecludedTent_053 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (PlaceClueOnLocationCost (Static 1))

instance RunMessage SecludedTent_053 where
  runMessage msg l@(SecludedTent_053 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ InvestigatorAt (be attrs)
      chooseTargetM iid investigators \chosen ->
        endOfPhaseModifier MythosPhase (attrs.ability 1) chosen (CannotBeAttackedBy "Towering Dark Young")
      pure l
    _ -> SecludedTent_053 <$> liftRunMessage msg attrs
