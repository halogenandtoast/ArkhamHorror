module Arkham.Asset.Cards.ZoeysCross (
  ZoeysCross (..),
  zoeysCross,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetCard ZoeysCross
zoeysCross = asset ZoeysCross Cards.zoeysCross

instance HasAbilities ZoeysCross where
  getAbilities (ZoeysCross x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy)
        $ Costs [ExhaustCost (toTarget x), ResourceCost 1]
    ]

instance RunMessage ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 [(windowType -> Window.EnemyEngaged _ eid)] _ -> do
      push $ EnemyDamage eid $ nonAttack attrs 1
      pure a
    _ -> ZoeysCross <$> runMessage msg attrs
