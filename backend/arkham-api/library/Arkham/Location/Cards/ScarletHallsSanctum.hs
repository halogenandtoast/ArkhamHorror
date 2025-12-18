module Arkham.Location.Cards.ScarletHallsSanctum (scarletHallsSanctum) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Conspirator))

newtype ScarletHallsSanctum = ScarletHallsSanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scarletHallsSanctum :: LocationCard ScarletHallsSanctum
scarletHallsSanctum = location ScarletHallsSanctum Cards.scarletHallsSanctum 2 (Static 0)

instance HasAbilities ScarletHallsSanctum where
  getAbilities (ScarletHallsSanctum a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ FastAbility (ExhaustAssetCost $ AssetWithTrait Conspirator <> AssetControlledBy You)

instance RunMessage ScarletHallsSanctum where
  runMessage msg l@(ScarletHallsSanctum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 2) iid
      pure l
    _ -> ScarletHallsSanctum <$> liftRunMessage msg attrs
