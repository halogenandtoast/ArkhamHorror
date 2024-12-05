module Arkham.Location.Cards.PassengerCar_169 (passengerCar_169, PassengerCar_169 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_169)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window

newtype PassengerCar_169 = PassengerCar_169 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_169 :: LocationCard PassengerCar_169
passengerCar_169 =
  locationWith PassengerCar_169 Cards.passengerCar_169 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor PassengerCar_169 where
  getModifiersFor (PassengerCar_169 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities PassengerCar_169 where
  getAbilities (PassengerCar_169 x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ Enters #after You (be x)

instance RunMessage PassengerCar_169 where
  runMessage msg l@(PassengerCar_169 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = SkillIconCost 2 (singleton #willpower)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost

      if hasSkills
        then chooseOneM iid do
          labeled "Take 2 horror" $ assignHorror iid (attrs.ability 1) 2
          labeled "Discard cards with at least 2 {willpower} icons" do
            push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignHorror iid (attrs.ability 1) 2
      pure l
    _ -> PassengerCar_169 <$> liftRunMessage msg attrs
