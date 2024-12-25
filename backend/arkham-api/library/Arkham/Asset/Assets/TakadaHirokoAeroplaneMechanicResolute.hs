module Arkham.Asset.Assets.TakadaHirokoAeroplaneMechanicResolute (
  takadaHirokoAeroplaneMechanicResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)

newtype TakadaHirokoAeroplaneMechanicResolute = TakadaHirokoAeroplaneMechanicResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takadaHirokoAeroplaneMechanicResolute :: AssetCard TakadaHirokoAeroplaneMechanicResolute
takadaHirokoAeroplaneMechanicResolute =
  allyWith
    TakadaHirokoAeroplaneMechanicResolute
    Cards.takadaHirokoAeroplaneMechanicResolute
    (3, 4)
    noSlots

instance HasAbilities TakadaHirokoAeroplaneMechanicResolute where
  getAbilities (TakadaHirokoAeroplaneMechanicResolute a) =
    [controlled a 1 (thisIs a (AssetWithUses Resource)) $ actionAbilityWithCost (exhaust a)]

instance RunMessage TakadaHirokoAeroplaneMechanicResolute where
  runMessage msg a@(TakadaHirokoAeroplaneMechanicResolute attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 1
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
    _ -> TakadaHirokoAeroplaneMechanicResolute <$> liftRunMessage msg attrs
