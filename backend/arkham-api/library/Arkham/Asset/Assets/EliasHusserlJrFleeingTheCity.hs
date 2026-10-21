module Arkham.Asset.Assets.EliasHusserlJrFleeingTheCity (eliasHusserlJrFleeingTheCity) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype EliasHusserlJrFleeingTheCity = EliasHusserlJrFleeingTheCity AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliasHusserlJrFleeingTheCity :: AssetCard EliasHusserlJrFleeingTheCity
eliasHusserlJrFleeingTheCity = allyWith EliasHusserlJrFleeingTheCity Cards.eliasHusserlJrFleeingTheCity (2, 2) noSlots

instance HasAbilities EliasHusserlJrFleeingTheCity where
  getAbilities (EliasHusserlJrFleeingTheCity a) = [restricted a 1 OnSameLocation parleyAction_]

instance RunMessage EliasHusserlJrFleeingTheCity where
  runMessage msg a@(EliasHusserlJrFleeingTheCity attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeLocationIfNotInPlay_ Locations.trainStation
      selectForMaybeM (locationIs Locations.trainStation) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.eliasHusserlJr"
      pure a
    _ -> EliasHusserlJrFleeingTheCity <$> liftRunMessage msg attrs
