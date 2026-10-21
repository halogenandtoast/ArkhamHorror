module Arkham.Asset.Assets.VicarYangTryingToDoTheRightThing (vicarYangTryingToDoTheRightThing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype VicarYangTryingToDoTheRightThing = VicarYangTryingToDoTheRightThing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vicarYangTryingToDoTheRightThing :: AssetCard VicarYangTryingToDoTheRightThing
vicarYangTryingToDoTheRightThing = allyWith VicarYangTryingToDoTheRightThing Cards.vicarYangTryingToDoTheRightThing (2, 2) noSlots

instance HasAbilities VicarYangTryingToDoTheRightThing where
  getAbilities (VicarYangTryingToDoTheRightThing a) = [restricted a 1 OnSameLocation parleyAction_]

instance RunMessage VicarYangTryingToDoTheRightThing where
  runMessage msg a@(VicarYangTryingToDoTheRightThing attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectForMaybeM (locationIs Locations.southChurch) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.vicarYang"
      pure a
    _ -> VicarYangTryingToDoTheRightThing <$> liftRunMessage msg attrs
