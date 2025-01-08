module Arkham.Location.Cards.PassengerCar_168 (passengerCar_168, PassengerCar_168 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_168)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window

newtype PassengerCar_168 = PassengerCar_168 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_168 :: LocationCard PassengerCar_168
passengerCar_168 =
  locationWith PassengerCar_168 Cards.passengerCar_168 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor PassengerCar_168 where
  getModifiersFor (PassengerCar_168 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities PassengerCar_168 where
  getAbilities (PassengerCar_168 x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ Enters #after You (be x)

instance RunMessage PassengerCar_168 where
  runMessage msg l@(PassengerCar_168 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = SkillIconCost 2 (singleton #combat)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost

      if hasSkills
        then chooseOneM iid do
          labeled "Take 2 damage" $ assignDamage iid (attrs.ability 1) 2
          labeled "Discard cards with at least 2 {combat} icons" do
            push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> PassengerCar_168 <$> liftRunMessage msg attrs
