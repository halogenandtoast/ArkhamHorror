module Arkham.Asset.Assets.GrotesqueStatue2 ( grotesqueStatue2,) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GrotesqueStatue2 = GrotesqueStatue2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grotesqueStatue2 :: AssetCard GrotesqueStatue2
grotesqueStatue2 =
  assetWith GrotesqueStatue2 Cards.grotesqueStatue2 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities GrotesqueStatue2 where
  getAbilities (GrotesqueStatue2 x) =
    [ restricted x 1 ControlsThis
        $ ReactionAbility (WouldRevealChaosToken #when You)
        $ assetUseCost x Charge 1
    ]

toDrawSource :: [Window] -> Source
toDrawSource [] = error "missing draw source"
toDrawSource ((windowType -> Window.WouldRevealChaosToken drawSource _) : _) = drawSource
toDrawSource (_ : rest) = toDrawSource rest

instance RunMessage GrotesqueStatue2 where
  runMessage msg a@(GrotesqueStatue2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (toDrawSource -> drawSource) _ -> do
      checkWhen $ Window.WouldRevealChaosTokens drawSource iid
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    _ -> GrotesqueStatue2 <$> liftRunMessage msg attrs
