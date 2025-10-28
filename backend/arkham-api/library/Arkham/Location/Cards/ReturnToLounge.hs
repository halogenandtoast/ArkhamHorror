module Arkham.Location.Cards.ReturnToLounge (returnToLounge) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToLounge = ReturnToLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToLounge :: LocationCard ReturnToLounge
returnToLounge = location ReturnToLounge Cards.returnToLounge 2 (PerPlayer 2)

instance HasAbilities ReturnToLounge where
  getAbilities (ReturnToLounge a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 Here $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      ]

instance RunMessage ReturnToLounge where
  runMessage msg l@(ReturnToLounge attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocation_ Cards.vault
      placeSetAsideLocation_ Cards.library
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeSetAsideLocation_ Cards.relicStorage
      pure l
    _ -> ReturnToLounge <$> liftRunMessage msg attrs
