module Arkham.Asset.Cards.TheRedClockBrokenButReliable5 (
  theRedClockBrokenButReliable5,
  TheRedClockBrokenButReliable5 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (getCanMoveToLocations)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Movement

newtype TheRedClockBrokenButReliable5 = TheRedClockBrokenButReliable5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedClockBrokenButReliable5 :: AssetCard TheRedClockBrokenButReliable5
theRedClockBrokenButReliable5 = asset TheRedClockBrokenButReliable5 Cards.theRedClockBrokenButReliable5

instance HasAbilities TheRedClockBrokenButReliable5 where
  getAbilities (TheRedClockBrokenButReliable5 a) = [restrictedAbility a 1 ControlsThis $ forced $ TurnBegins #after You]

instance RunMessage TheRedClockBrokenButReliable5 where
  runMessage msg a@(TheRedClockBrokenButReliable5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let charges = attrs.use Charge
      when (charges > 0)
        $ chooseOrRunOne
          iid
          [ Label
              "Take all charges here as resources"
              [MoveTokens (attrs.ability 1) (toSource attrs) (ResourceTarget iid) Charge charges]
          , Label "Leave charges" []
          ]

      pushAll [AddUses (attrs.ability 1) attrs.id Charge 1, Do msg]
      pure a
    Do msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.use Charge of
        1 -> nextSkillTestModifier (attrs.ability 1) iid (AnySkillValue 4)
        2 -> doStep 3 msg'
        3 -> push $ GainActions iid (attrs.ability 1) 2
        _ -> pure ()
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
    _ -> TheRedClockBrokenButReliable5 <$> liftRunMessage msg attrs
