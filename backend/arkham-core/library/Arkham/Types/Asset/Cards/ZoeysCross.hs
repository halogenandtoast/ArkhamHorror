module Arkham.Types.Asset.Cards.ZoeysCross
  ( ZoeysCross(..)
  , zoeysCross
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetCard ZoeysCross
zoeysCross = accessory ZoeysCross Cards.zoeysCross

instance HasAbilities ZoeysCross where
  getAbilities (ZoeysCross x) =
    [ restrictedAbility x 1 OwnsThis
        $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy)
        $ Costs [ExhaustCost (toTarget x), ResourceCost 1]
    ]

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source [Window _ (Window.EnemyEngaged _ eid)] 1 _
      | isSource attrs source -> a
      <$ push (EnemyDamage eid iid source NonAttackDamageEffect 1)
    _ -> ZoeysCross <$> runMessage msg attrs
