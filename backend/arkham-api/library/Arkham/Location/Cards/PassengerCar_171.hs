module Arkham.Location.Cards.PassengerCar_171 (passengerCar_171, PassengerCar_171 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_171)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window

newtype PassengerCar_171 = PassengerCar_171 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_171 :: LocationCard PassengerCar_171
passengerCar_171 =
  locationWith PassengerCar_171 Cards.passengerCar_171 1 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor PassengerCar_171 where
  getModifiersFor (PassengerCar_171 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities PassengerCar_171 where
  getAbilities (PassengerCar_171 x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ Enters #after You (be x)

instance RunMessage PassengerCar_171 where
  runMessage msg l@(PassengerCar_171 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = SkillIconCost 1 (singleton #wild)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost

      if hasSkills
        then chooseOneM iid do
          labeled "Take 1 damage and 1 horror" $ assignDamageAndHorror iid (attrs.ability 1) 1 1
          labeled "Discard cards with at least 1 {wild} icons" do
            push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure l
    _ -> PassengerCar_171 <$> liftRunMessage msg attrs
