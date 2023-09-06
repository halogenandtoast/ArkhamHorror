module Arkham.Asset.Cards.GrotesqueStatue2 (
  GrotesqueStatue2 (..),
  grotesqueStatue2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBagStepState
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype GrotesqueStatue2 = GrotesqueStatue2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueStatue2 :: AssetCard GrotesqueStatue2
grotesqueStatue2 =
  assetWith GrotesqueStatue2 Cards.grotesqueStatue2 (discardWhenNoUsesL .~ True)

instance HasAbilities GrotesqueStatue2 where
  getAbilities (GrotesqueStatue2 x) =
    [ restrictedAbility x 1 ControlsThis $
        ReactionAbility (WouldRevealChaosToken Timing.When You) $
          UseCost (AssetWithId $ toId x) Charge 1
    ]

instance RunMessage GrotesqueStatue2 where
  runMessage msg a@(GrotesqueStatue2 attrs) = case msg of
    UseCardAbility iid source 1 [Window Timing.When (Window.WouldRevealChaosToken drawSource _) _] _
      | isSource attrs source -> do
          ignoreWindow <-
            checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect source)]
          pushAll
            [ ReplaceCurrentDraw drawSource iid $
                Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] []
            , ignoreWindow
            ]
          pure a
    _ -> GrotesqueStatue2 <$> runMessage msg attrs
