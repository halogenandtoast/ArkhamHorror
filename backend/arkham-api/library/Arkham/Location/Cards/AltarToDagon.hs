module Arkham.Location.Cards.AltarToDagon (altarToDagon, AltarToDagon (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AltarToDagon = AltarToDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

altarToDagon :: LocationCard AltarToDagon
altarToDagon = locationWith AltarToDagon Cards.altarToDagon 3 (Static 0) connectsToAdjacent

instance HasAbilities AltarToDagon where
  getAbilities (AltarToDagon attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          ( Here
              <> KeyIsSetAside GreenKey
              <> oneOf
                [ exists (investigatorAt attrs <> InvestigatorWithKey BlueKey)
                , HasCalculation (InvestigatorKeyCountCalculation (investigatorAt attrs)) (atLeast 3)
                ]
          )
          $ actionAbilityWithCost
          $ GroupClueCost (PerPlayer 2) (be attrs)
      ]

instance RunMessage AltarToDagon where
  runMessage msg l@(AltarToDagon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placeKey iid GreenKey
      pure l
    _ -> AltarToDagon <$> liftRunMessage msg attrs
