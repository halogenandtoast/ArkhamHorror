module Arkham.Location.Cards.PassengerCar_170 (passengerCar_170, PassengerCar_170 (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (passengerCar_170)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window

newtype PassengerCar_170 = PassengerCar_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passengerCar_170 :: LocationCard PassengerCar_170
passengerCar_170 =
  locationWith PassengerCar_170 Cards.passengerCar_170 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasModifiersFor PassengerCar_170 where
  getModifiersFor (PassengerCar_170 l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities PassengerCar_170 where
  getAbilities (PassengerCar_170 x) =
    extendRevealed1 x $ restricted x 1 Here $ forced $ Enters #after You (be x)

instance RunMessage PassengerCar_170 where
  runMessage msg l@(PassengerCar_170 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let cost = SkillIconCost 2 (singleton #intellect)
      hasSkills <- getCanAffordCost iid (toAbilitySource attrs 1) [] [mkWhen NonFast] cost

      if hasSkills
        then chooseOneM iid do
          labeled "Take 2 horror" $ assignHorror iid (attrs.ability 1) 2
          labeled "Discard cards with at least 2 {intellect} icons" do
            push $ PayForAbility (abilityEffect attrs [] cost) []
        else assignHorror iid (attrs.ability 1) 2
      pure l
    _ -> PassengerCar_170 <$> liftRunMessage msg attrs
