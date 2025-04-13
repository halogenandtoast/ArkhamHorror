module Arkham.Location.Cards.ReturnToOsbornsGeneralStore (returnToOsbornsGeneralStore) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToOsbornsGeneralStore = ReturnToOsbornsGeneralStore LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToOsbornsGeneralStore :: LocationCard ReturnToOsbornsGeneralStore
returnToOsbornsGeneralStore =
  locationWith
    ReturnToOsbornsGeneralStore
    Cards.returnToOsbornsGeneralStore
    3
    (PerPlayer 1)
    (labelL .~ "osbornsGeneralStore")

instance HasAbilities ReturnToOsbornsGeneralStore where
  getAbilities (ReturnToOsbornsGeneralStore a) =
    withDrawCardUnderneathAction a
      <> extendRevealed1
        a
        (restricted a 1 (youExist can.gain.resources) $ forced $ RevealLocation #after You (be a))

instance RunMessage ReturnToOsbornsGeneralStore where
  runMessage msg l@(ReturnToOsbornsGeneralStore attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure l
    _ -> ReturnToOsbornsGeneralStore <$> liftRunMessage msg attrs
