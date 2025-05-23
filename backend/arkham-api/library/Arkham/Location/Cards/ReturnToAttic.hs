module Arkham.Location.Cards.ReturnToAttic (returnToAttic) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToAttic = ReturnToAttic LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToAttic :: LocationCard ReturnToAttic
returnToAttic = location ReturnToAttic Cards.returnToAttic 3 (PerPlayer 1)

instance HasAbilities ReturnToAttic where
  getAbilities (ReturnToAttic a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage ReturnToAttic where
  runMessage msg l@(ReturnToAttic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocation_ Cards.farAboveYourHouse
      pure l
    _ -> ReturnToAttic <$> liftRunMessage msg attrs
