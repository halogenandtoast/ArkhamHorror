module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.StorageNight (storageNight) where

import Arkham.Ability
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype StorageNight = StorageNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

storageNight :: LocationCard StorageNight
storageNight = symbolLabel $ location StorageNight Cards.storageNight 4 (PerPlayer 1)

instance HasAbilities StorageNight where
  getAbilities (StorageNight a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ FastAbility (AddTokenCost 1 #blood)

instance RunMessage StorageNight where
  runMessage msg l@(StorageNight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember TheInvestigatorsFoundTheManagersKeys
      pure l
    _ -> StorageNight <$> liftRunMessage msg attrs
