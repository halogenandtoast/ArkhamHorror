module Arkham.Asset.Cards.TheGoldPocketWatch4 (
  theGoldPocketWatch4,
  TheGoldPocketWatch4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheGoldPocketWatch4 = TheGoldPocketWatch4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theGoldPocketWatch4 :: AssetCard TheGoldPocketWatch4
theGoldPocketWatch4 = asset TheGoldPocketWatch4 Cards.theGoldPocketWatch4

instance HasAbilities TheGoldPocketWatch4 where
  getAbilities (TheGoldPocketWatch4 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (PhaseBegins Timing.When AnyPhase) Free
    , restrictedAbility attrs 2 ControlsThis
        $ ReactionAbility (PhaseEnds Timing.When AnyPhase) Free
    ]

instance RunMessage TheGoldPocketWatch4 where
  runMessage msg a@(TheGoldPocketWatch4 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      pushAll [RemoveFromGame (toTarget attrs), EndPhase]
      pure a
    UseCardAbility _ source 2 [(windowType -> Window.PhaseEnds phase)] _ | isSource attrs source -> do
      clearQueue
      pushAll [RemoveFromGame (toTarget attrs), Begin phase]
      pure a
    _ -> TheGoldPocketWatch4 <$> runMessage msg attrs
