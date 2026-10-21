module Arkham.Asset.Assets.LinaHusserlTheNextBigThing (linaHusserlTheNextBigThing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype LinaHusserlTheNextBigThing = LinaHusserlTheNextBigThing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

linaHusserlTheNextBigThing :: AssetCard LinaHusserlTheNextBigThing
linaHusserlTheNextBigThing = allyWith LinaHusserlTheNextBigThing Cards.linaHusserlTheNextBigThing (2, 2) noSlots

instance HasAbilities LinaHusserlTheNextBigThing where
  getAbilities (LinaHusserlTheNextBigThing a) =
    [restricted a 1 (OnSameLocation <> Remembered TheInvestigatorsGlimpsedAnotherWorld) parleyAction_]

instance RunMessage LinaHusserlTheNextBigThing where
  runMessage msg a@(LinaHusserlTheNextBigThing attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectForMaybeM (locationIs Locations.merchantDistrict) $ place attrs . AtLocation
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      codexEntry "codex.linaHusserl"
      remember LinaOpenedTheDoor
      scenarioSpecific "theCulprit" ()
      pure a
    _ -> LinaHusserlTheNextBigThing <$> liftRunMessage msg attrs
