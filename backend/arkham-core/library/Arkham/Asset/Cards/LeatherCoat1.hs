module Arkham.Asset.Cards.LeatherCoat1
  ( leatherCoat1
  , LeatherCoat1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( AssetDefeated )
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LeatherCoat1 = LeatherCoat1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leatherCoat1 :: AssetCard LeatherCoat1
leatherCoat1 = assetWith LeatherCoat1 Cards.leatherCoat1 (healthL ?~ 4)

instance HasAbilities LeatherCoat1 where
  getAbilities (LeatherCoat1 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ AssetDefeated Timing.When ByDamage
        $ AssetWithId
        $ toId a
    ]

instance RunMessage LeatherCoat1 where
  runMessage msg a@(LeatherCoat1 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Exile (toTarget attrs)
      pure a
    _ -> LeatherCoat1 <$> runMessage msg attrs
