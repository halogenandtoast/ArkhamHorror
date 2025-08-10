module Arkham.Asset.Assets.TheRedClockBrokenButReliable2 (theRedClockBrokenButReliable2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype TheRedClockBrokenButReliable2 = TheRedClockBrokenButReliable2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedClockBrokenButReliable2 :: AssetCard TheRedClockBrokenButReliable2
theRedClockBrokenButReliable2 = asset TheRedClockBrokenButReliable2 Cards.theRedClockBrokenButReliable2

instance HasAbilities TheRedClockBrokenButReliable2 where
  getAbilities (TheRedClockBrokenButReliable2 a) =
    [restricted a 1 ControlsThis $ forced $ TurnBegins #after You]

instance RunMessage TheRedClockBrokenButReliable2 where
  runMessage msg a@(TheRedClockBrokenButReliable2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let charges = attrs.use Charge
      chooseOrRunOneM iid do
        labeled "Place 1 charge here" do
          addUses (attrs.ability 1) attrs.id Charge 1
          do_ msg

        when (charges > 0) do
          labeled "Take all charges here as resources"
            $ moveTokens (attrs.ability 1) attrs (ResourceTarget iid) Charge charges
      pure a
    Do msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.use Charge of
        1 -> nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue 3)
        2 -> doStep 2 msg'
        3 -> gainActions iid (attrs.ability 1) 1
        _ -> pure ()
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      locations <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseOrRunOneM iid do
        labeled "Do not move" nothing
        targets locations \location -> do
          moveTo attrs iid location
          doStep (n - 1) msg'
      pure a
    _ -> TheRedClockBrokenButReliable2 <$> liftRunMessage msg attrs
