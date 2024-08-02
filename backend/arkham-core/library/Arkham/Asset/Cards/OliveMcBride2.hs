module Arkham.Asset.Cards.OliveMcBride2 (
  oliveMcBride2,
  OliveMcBride2 (..),
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

newtype OliveMcBride2 = OliveMcBride2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oliveMcBride2 :: AssetCard OliveMcBride2
oliveMcBride2 = ally OliveMcBride2 Cards.oliveMcBride2 (1, 3)

instance HasAbilities OliveMcBride2 where
  getAbilities (OliveMcBride2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (WouldRevealChaosToken Timing.When You)
        $ ExhaustCost (toTarget a)
    ]

instance RunMessage OliveMcBride2 where
  runMessage msg a@(OliveMcBride2 attrs) = case msg of
    UseCardAbility
      iid
      (isSource attrs -> True)
      1
      [Window Timing.When (Window.WouldRevealChaosToken drawSource _) _]
      _ ->
        do
          ignoreWindow <-
            checkWindows
              [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toAbilitySource attrs 1)]
          pushAll
            [ ReplaceCurrentDraw drawSource iid
                $ Choose
                  (toSource attrs)
                  2
                  ResolveChoice
                  [Undecided Draw, Undecided Draw, Undecided Draw, Undecided Draw]
                  []
            , ignoreWindow
            ]
          pure a
    _ -> OliveMcBride2 <$> runMessage msg attrs
