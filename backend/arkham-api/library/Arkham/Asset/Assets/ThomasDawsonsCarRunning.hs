module Arkham.Asset.Assets.ThomasDawsonsCarRunning (
  thomasDawsonsCarRunning,
  ThomasDawsonsCarRunning (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Name
import Arkham.Placement
import Arkham.Trait (Trait (Road))
import Arkham.Window qualified as Window

newtype ThomasDawsonsCarRunning = ThomasDawsonsCarRunning AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasDawsonsCarRunning :: AssetCard ThomasDawsonsCarRunning
thomasDawsonsCarRunning = asset ThomasDawsonsCarRunning Cards.thomasDawsonsCarRunning

instance HasAbilities ThomasDawsonsCarRunning where
  getAbilities (ThomasDawsonsCarRunning x) =
    [ withTooltip
        "If you are this vehicle's driver: Draw the top card of the encounter deck. Then, move this vehicle to a connecting _Road_ location. (Max once per round.)"
        $ groupLimit PerRound
        $ restrictedAbility x 1 driverCriteria actionAbility
    , withTooltip "If you are this vehicle's driver: You stop the car. Flip this vehicle over."
        $ restrictedAbility x 2 driverCriteria actionAbility
    ]
   where
    driverCriteria = maybe Never (youExist . InvestigatorWithId) x.driver

instance RunMessage ThomasDawsonsCarRunning where
  runMessage msg a@(ThomasDawsonsCarRunning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      whenM (withoutModifier attrs VehicleCannotMove) do
        getLocationOf attrs.id >>= traverse_ \lid -> do
          roads <- select $ withTrait Road <> ConnectedTo (LocationWithId lid)
          chooseTargetM iid roads $ place attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOver iid attrs
      pure a
    PlaceInvestigator iid (InVehicle aid) | aid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . ThomasDawsonsCarRunning $ attrs' & driverL %~ Just . fromMaybe iid
    PlaceInvestigator iid _ | Just iid == attrs.driver -> do
      attrs' <- liftRunMessage msg attrs
      passengers <- select $ InVehicleMatching (be attrs)
      for_ (headMay passengers) \p -> do
        chooseOrRunOneM p do
          questionLabeled $ "Choose new driver for " <> toTitle attrs.name
          targets passengers $ push . SetDriver attrs.id
      pure . ThomasDawsonsCarRunning $ attrs' & driverL .~ Nothing
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thomasDawsonsCarStopped
      pure a
    PlaceAsset aid (AtLocation lid) | aid == attrs.id -> do
      case attrs.placement of
        AtLocation lid' | lid /= lid' -> do
          checkWhen $ Window.VehicleLeaves attrs.id lid'
          push $ Do msg
          pure a
        _ -> ThomasDawsonsCarRunning <$> liftRunMessage msg attrs
    Do msg'@(PlaceAsset aid (AtLocation _lid)) | aid == attrs.id -> do
      ThomasDawsonsCarRunning <$> liftRunMessage msg' attrs
    _ -> ThomasDawsonsCarRunning <$> liftRunMessage msg attrs
