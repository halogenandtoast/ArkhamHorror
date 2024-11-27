module Arkham.Asset.Assets.TakadaHirokoAeroplaneMechanic (
  takadaHirokoAeroplaneMechanic,
  TakadaHirokoAeroplaneMechanic (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)

newtype TakadaHirokoAeroplaneMechanic = TakadaHirokoAeroplaneMechanic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takadaHirokoAeroplaneMechanic :: AssetCard TakadaHirokoAeroplaneMechanic
takadaHirokoAeroplaneMechanic = allyWith TakadaHirokoAeroplaneMechanic Cards.takadaHirokoAeroplaneMechanic (3, 3) noSlots

instance HasAbilities TakadaHirokoAeroplaneMechanic where
  getAbilities (TakadaHirokoAeroplaneMechanic a) =
    [controlled a 1 (thisIs a (AssetWithUses Resource)) $ actionAbilityWithCost (exhaust a)]

instance RunMessage TakadaHirokoAeroplaneMechanic where
  runMessage msg a@(TakadaHirokoAeroplaneMechanic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAmounts
        iid
        "Resources"
        (MaxAmountTarget 3)
        [("Resources", (0, min 3 (attrs.use Resource)))]
        attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) -> do
      moveTokens (attrs.ability 1) attrs iid Resource n
      pure a
    _ -> TakadaHirokoAeroplaneMechanic <$> liftRunMessage msg attrs
