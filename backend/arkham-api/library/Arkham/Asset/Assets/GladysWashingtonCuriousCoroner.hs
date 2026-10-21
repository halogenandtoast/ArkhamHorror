module Arkham.Asset.Assets.GladysWashingtonCuriousCoroner (gladysWashingtonCuriousCoroner) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype GladysWashingtonCuriousCoroner = GladysWashingtonCuriousCoroner AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gladysWashingtonCuriousCoroner :: AssetCard GladysWashingtonCuriousCoroner
gladysWashingtonCuriousCoroner =
  allyWith GladysWashingtonCuriousCoroner Cards.gladysWashingtonCuriousCoroner (2, 2) noSlots

instance HasAbilities GladysWashingtonCuriousCoroner where
  getAbilities (GladysWashingtonCuriousCoroner a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage GladysWashingtonCuriousCoroner where
  runMessage msg a@(GladysWashingtonCuriousCoroner attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.duterteFuneralHomeAfterHours
      selectForMaybeM (locationIs Locations.duterteFuneralHomeAfterHours)
        $ place attrs
        . AtLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      witnessParley sid iid attrs #agility
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      codexEntry "codex.gladysWashington"
      sealSymbolAndClaim attrs #skull
      pure a
    _ -> GladysWashingtonCuriousCoroner <$> liftRunMessage msg attrs
