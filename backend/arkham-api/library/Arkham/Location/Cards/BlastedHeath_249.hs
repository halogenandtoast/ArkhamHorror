module Arkham.Location.Cards.BlastedHeath_249 (blastedHeath_249) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (blastedHeath_249)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BlastedHeath_249 = BlastedHeath_249 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_249 :: LocationCard BlastedHeath_249
blastedHeath_249 = location BlastedHeath_249 Cards.blastedHeath_249 3 (Static 2)

instance HasAbilities BlastedHeath_249 where
  getAbilities (BlastedHeath_249 a) = extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage BlastedHeath_249 where
  runMessage msg l@(BlastedHeath_249 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> BlastedHeath_249 <$> liftRunMessage msg attrs
