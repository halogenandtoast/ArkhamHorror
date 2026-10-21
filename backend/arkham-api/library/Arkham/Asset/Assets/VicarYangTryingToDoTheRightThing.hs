module Arkham.Asset.Assets.VicarYangTryingToDoTheRightThing (vicarYangTryingToDoTheRightThing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- "Choose an investigator at Vicar Yang's location to test {intellect} (3).
      If they succeed, put the set-aside Meiger Estate (Under New Ownership)
      location into play, and add Vicar Yang to the victory display. Otherwise,
      any investigator may trigger this codex again." -}
      investigators <- select $ investigatorAt (locationWithAsset attrs)
      twoPartCodex iid "codex.vicarYang" "vicarYang" $ targets investigators \chosen -> do
        sid <- getRandom
        beginSkillTest sid chosen (attrs.ability 1) chosen #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeLocationIfNotInPlay_ Locations.meigerEstateUnderNewOwnership
      addToVictory_ attrs
      pure a
    _ -> VicarYangTryingToDoTheRightThing <$> liftRunMessage msg attrs
