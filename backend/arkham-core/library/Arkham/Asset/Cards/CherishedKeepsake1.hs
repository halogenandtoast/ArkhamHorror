module Arkham.Asset.Cards.CherishedKeepsake1 (
  cherishedKeepsake1,
  CherishedKeepsake1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (AssetDefeated)
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype CherishedKeepsake1 = CherishedKeepsake1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cherishedKeepsake1 :: AssetCard CherishedKeepsake1
cherishedKeepsake1 =
  assetWith CherishedKeepsake1 Cards.cherishedKeepsake1 (sanityL ?~ 4)

instance HasAbilities CherishedKeepsake1 where
  getAbilities (CherishedKeepsake1 a) =
    [ restrictedAbility a 1 ControlsThis $
        ForcedAbility $
          AssetDefeated Timing.When ByHorror $
            AssetWithId $
              toId a
    ]

instance RunMessage CherishedKeepsake1 where
  runMessage msg a@(CherishedKeepsake1 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ Exile (toTarget attrs)
      pure a
    _ -> CherishedKeepsake1 <$> runMessage msg attrs
