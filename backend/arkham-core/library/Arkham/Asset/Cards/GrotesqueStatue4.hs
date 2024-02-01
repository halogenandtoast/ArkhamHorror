module Arkham.Asset.Cards.GrotesqueStatue4 (
  GrotesqueStatue4 (..),
  grotesqueStatue4,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosBagStepState
import Arkham.Matcher
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype GrotesqueStatue4 = GrotesqueStatue4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

grotesqueStatue4 :: AssetCard GrotesqueStatue4
grotesqueStatue4 = assetWith GrotesqueStatue4 Cards.grotesqueStatue4 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities GrotesqueStatue4 where
  getAbilities (GrotesqueStatue4 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility (WouldRevealChaosToken #when You)
        $ assetUseCost x Charge 1
    ]

toDrawSource :: [Window] -> Source
toDrawSource [] = error "missing draw source"
toDrawSource ((windowType -> Window.WouldRevealChaosToken drawSource _) : _) = drawSource
toDrawSource (_ : rest) = toDrawSource rest

instance RunMessage GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toDrawSource -> drawSource) _ -> do
      ignoreWindow <-
        checkWindows [mkWindow #after (Window.CancelledOrIgnoredCardOrGameEffect (toSource attrs))]
      pushAll
        [ ReplaceCurrentDraw drawSource iid
            $ Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] []
        , ignoreWindow
        ]
      pure a
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
