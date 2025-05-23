module Arkham.Asset.Assets.GrotesqueStatue4 (grotesqueStatue4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBagStepState
import Arkham.Helpers.Window (getDrawSource)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype GrotesqueStatue4 = GrotesqueStatue4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueStatue4 :: AssetCard GrotesqueStatue4
grotesqueStatue4 = assetWith GrotesqueStatue4 Cards.grotesqueStatue4 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities GrotesqueStatue4 where
  getAbilities (GrotesqueStatue4 x) =
    [ restricted x 1 ControlsThis
        $ triggered (WouldRevealChaosToken #when You)
        $ assetUseCost x Charge 1
    ]

instance RunMessage GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      checkWhen $ Window.WouldRevealChaosTokens drawSource iid
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> GrotesqueStatue4 <$> liftRunMessage msg attrs
