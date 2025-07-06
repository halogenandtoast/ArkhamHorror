module Arkham.Location.Cards.BaggageCar (baggageCar) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BaggageCar = BaggageCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baggageCar :: LocationCard BaggageCar
baggageCar = locationWith BaggageCar Cards.baggageCar 3 (Static 3) connectsToAdjacent

instance HasModifiersFor BaggageCar where
  getModifiersFor (BaggageCar a) = do
    whenUnrevealed a $ blockedWhenAny a $ leftOf a <> LocationWithAnyClues
    modifySelect a Anyone [CannotInvestigateLocation a.id]

instance HasAbilities BaggageCar where
  getAbilities (BaggageCar a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithAnyClues)
      $ actionAbilityWithCost (HandDiscardCost 1 #any)

instance RunMessage BaggageCar where
  runMessage msg l@(BaggageCar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> BaggageCar <$> liftRunMessage msg attrs
