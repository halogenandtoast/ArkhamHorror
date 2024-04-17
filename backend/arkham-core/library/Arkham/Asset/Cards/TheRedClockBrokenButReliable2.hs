module Arkham.Asset.Cards.TheRedClockBrokenButReliable2 (
  theRedClockBrokenButReliable2,
  TheRedClockBrokenButReliable2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (getCanMoveToLocations)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Movement

newtype TheRedClockBrokenButReliable2 = TheRedClockBrokenButReliable2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedClockBrokenButReliable2 :: AssetCard TheRedClockBrokenButReliable2
theRedClockBrokenButReliable2 = asset TheRedClockBrokenButReliable2 Cards.theRedClockBrokenButReliable2

instance HasAbilities TheRedClockBrokenButReliable2 where
  getAbilities (TheRedClockBrokenButReliable2 a) = [restrictedAbility a 1 ControlsThis $ forced $ TurnBegins #after You]

instance RunMessage TheRedClockBrokenButReliable2 where
  runMessage msg a@(TheRedClockBrokenButReliable2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let charges = attrs.use Charge
      let
        otherMessages
          | charges == 1 = [Msg.skillTestModifier (attrs.ability 1) iid (AnySkillValue 3)]
          | charges == 2 = [DoStep 2 msg]
          | charges == 3 = [GainActions iid (attrs.ability 1) 1]
          | otherwise = []
      chooseOrRunOne
        iid
        $ [Label "Place 1 charge here" [AddUses attrs.id Charge 1]]
        <> [ Label
            "Take all charges here as resources"
            $ MoveUses (toSource attrs) (ResourceTarget iid) Charge charges
            : otherMessages
           | charges > 0
           ]
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      locations <- getCanMoveToLocations iid (attrs.ability 1)
      unless (null locations) $ do
        chooseOne
          iid
          $ Label "Do not move" []
          : [ targetLabel location [MoveTo $ move (toSource attrs) iid location, DoStep (n - 1) msg']
            | location <- locations
            ]
      pure a
    _ -> TheRedClockBrokenButReliable2 <$> lift (runMessage msg attrs)
