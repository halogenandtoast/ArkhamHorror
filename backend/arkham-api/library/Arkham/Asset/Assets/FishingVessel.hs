module Arkham.Asset.Assets.FishingVessel (fishingVessel, FishingVessel (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Projection
import Arkham.Trait (Trait (Ocean))

newtype FishingVessel = FishingVessel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishingVessel :: AssetCard FishingVessel
fishingVessel = asset FishingVessel Cards.fishingVessel

instance HasAbilities FishingVessel where
  getAbilities (FishingVessel x) =
    [ playerLimit PerRound
        $ restricted x 1 (oneOf [CanEnterThisVehicle, CanLeaveThisVehicle])
        $ FastAbility Free
    , restrictedAbility x 2 InThisVehicle actionAbility
    ]

instance RunMessage FishingVessel where
  runMessage msg a@(FishingVessel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placement <- field InvestigatorPlacement iid
      case placement of
        AtLocation _ -> place iid (InVehicle attrs.id)
        p@(InVehicle _) ->
          placementLocation p >>= \case
            Nothing -> error "No location for vehicle"
            Just lid -> place iid (AtLocation lid)
        _ -> error "Invalid placement"
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      getLocationOf attrs.id >>= \case
        Nothing -> pure ()
        Just lid -> do
          oceans <- select $ withTrait Ocean <> ConnectedTo (LocationWithId lid)
          chooseTargetM iid oceans $ place attrs
      pure a
    _ -> FishingVessel <$> liftRunMessage msg attrs
