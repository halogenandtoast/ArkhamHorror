module Arkham.Location.Cards.ReturnToCellar (returnToCellar) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToCellar = ReturnToCellar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCellar :: LocationCard ReturnToCellar
returnToCellar = location ReturnToCellar Cards.returnToCellar 2 (PerPlayer 1)

instance HasAbilities ReturnToCellar where
  getAbilities (ReturnToCellar a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage ReturnToCellar where
  runMessage msg l@(ReturnToCellar attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocation_ Cards.deepBelowYourHouse
      pure l
    _ -> ReturnToCellar <$> liftRunMessage msg attrs
