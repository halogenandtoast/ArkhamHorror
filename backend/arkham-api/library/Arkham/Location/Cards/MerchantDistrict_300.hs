module Arkham.Location.Cards.MerchantDistrict_300 (merchantDistrict_300) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus hiding (removeBreaches)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype MerchantDistrict_300 = MerchantDistrict_300 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

merchantDistrict_300 :: LocationCard MerchantDistrict_300
merchantDistrict_300 = location MerchantDistrict_300 Cards.merchantDistrict_300 2 (Static 0)

instance HasAbilities MerchantDistrict_300 where
  getAbilities (MerchantDistrict_300 a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ actionAbilityWithCost (OrCost [DiscardTopOfDeckCost n | n <- [5, 10, 15]])

instance RunMessage MerchantDistrict_300 where
  runMessage msg l@(MerchantDistrict_300 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (discardedCards -> cards) -> do
      let weaknesses = filter (`cardMatch` WeaknessCard) cards
      let n = min (maybe 0 countBreaches $ locationBreaches attrs) (length cards `div` 5)
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      addToHand iid weaknesses
      pure l
    _ -> MerchantDistrict_300 <$> liftRunMessage msg attrs
