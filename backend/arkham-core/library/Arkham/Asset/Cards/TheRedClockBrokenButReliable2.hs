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
      chooseOrRunOne
        iid
        $ [Label "Place 1 charge here" [AddUses (attrs.ability 1) attrs.id Charge 1, Do msg]]
        <> [ Label
            "Take all charges here as resources"
            [MoveTokens (attrs.ability 1) (toSource attrs) (ResourceTarget iid) Charge charges]
           | charges > 0
           ]
      pure a
    Do msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.use Charge of
        1 -> nextSkillTestModifier (attrs.ability 1) iid (AnySkillValue 3)
        2 -> doStep 2 msg'
        3 -> push $ GainActions iid (attrs.ability 1) 1
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
    _ -> TheRedClockBrokenButReliable2 <$> liftRunMessage msg attrs
