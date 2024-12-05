module Arkham.Location.Cards.PassengerCar_167 (passengerCar_167, PassengerCar_167 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_167)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window

newtype PassengerCar_167 = PassengerCar_167 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_167 :: LocationCard PassengerCar_167
passengerCar_167 =
  locationWith PassengerCar_167 Cards.passengerCar_167 1 (PerPlayer 3)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor PassengerCar_167 where
  getModifiersFor (PassengerCar_167 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities PassengerCar_167 where
  getAbilities (PassengerCar_167 x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ Enters #after You (be x)

instance RunMessage PassengerCar_167 where
  runMessage msg l@(PassengerCar_167 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = SkillIconCost 2 (singleton #agility)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost

      if hasSkills
        then chooseOneM iid do
          labeled "Take 2 damage" $ assignDamage iid (attrs.ability 1) 2
          labeled "Discard cards with at least 2 {agility} icons" do
            push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> PassengerCar_167 <$> liftRunMessage msg attrs
