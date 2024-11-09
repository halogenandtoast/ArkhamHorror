module Arkham.Location.Cards.DoorwayToTheDepths (doorwayToTheDepths, DoorwayToTheDepths (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.ScenarioLogKey

newtype DoorwayToTheDepths = DoorwayToTheDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheDepths :: LocationCard DoorwayToTheDepths
doorwayToTheDepths = location DoorwayToTheDepths Cards.doorwayToTheDepths 5 (Static 1)

instance HasAbilities DoorwayToTheDepths where
  getAbilities (DoorwayToTheDepths a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 Here
          $ actionAbilityWithCost
          $ GroupSpendKeyCost GreenKey (be a)
          <> GroupClueCost (PerPlayer 3) (be a)
      ]

instance RunMessage DoorwayToTheDepths where
  runMessage msg l@(DoorwayToTheDepths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ LocationWithMostClues Anywhere
      chooseOrRunOneM iid do
        targets ls \lid -> placeKey lid GreenKey
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember UnlockedTheFinalDepths
      pure l
    _ -> DoorwayToTheDepths <$> liftRunMessage msg attrs
