module Arkham.Asset.Assets.TheRedClockBrokenButReliable5 (theRedClockBrokenButReliable5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype TheRedClockBrokenButReliable5 = TheRedClockBrokenButReliable5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedClockBrokenButReliable5 :: AssetCard TheRedClockBrokenButReliable5
theRedClockBrokenButReliable5 = asset TheRedClockBrokenButReliable5 Cards.theRedClockBrokenButReliable5

instance HasAbilities TheRedClockBrokenButReliable5 where
  getAbilities (TheRedClockBrokenButReliable5 a) =
    [restricted a 1 ControlsThis $ forced $ TurnBegins #after You]

instance RunMessage TheRedClockBrokenButReliable5 where
  runMessage msg a@(TheRedClockBrokenButReliable5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let charges = attrs.use Charge
      when (charges > 0) do
        chooseOrRunOneM iid do
          labeled "Take all charges here as resources"
            $ moveTokens (attrs.ability 1) attrs (ResourceTarget iid) Charge charges
          labeled "Leave charges" nothing

      addUses (attrs.ability 1) attrs.id Charge 1
      do_ msg
      pure a
    Do msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.use Charge of
        1 -> nextSkillTestModifier iid (attrs.ability 1) iid (AnySkillValue 4)
        2 -> doStep 3 msg'
        3 -> gainActions iid (attrs.ability 1) 2
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
    _ -> TheRedClockBrokenButReliable5 <$> liftRunMessage msg attrs
