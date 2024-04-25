module Arkham.Asset.Cards.TheGoldPocketWatch4 (theGoldPocketWatch4, TheGoldPocketWatch4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheGoldPocketWatch4 = TheGoldPocketWatch4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGoldPocketWatch4 :: AssetCard TheGoldPocketWatch4
theGoldPocketWatch4 = asset TheGoldPocketWatch4 Cards.theGoldPocketWatch4

instance HasAbilities TheGoldPocketWatch4 where
  getAbilities (TheGoldPocketWatch4 attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ freeReaction (PhaseBegins #when AnyPhase)
    , restrictedAbility attrs 2 ControlsThis $ freeReaction (PhaseEnds #when AnyPhase)
    ]

instance RunMessage TheGoldPocketWatch4 where
  runMessage msg a@(TheGoldPocketWatch4 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pushAll [RemoveFromGame (toTarget attrs), EndPhase, After EndPhase]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 [windowType -> Window.PhaseEnds phase] _ -> do
      clearQueue
      pushAll [RemoveFromGame (toTarget attrs), Again (Begin phase), Begin phase]
      pure a
    _ -> TheGoldPocketWatch4 <$> runMessage msg attrs
