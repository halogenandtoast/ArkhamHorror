module Arkham.Types.Asset.Cards.ZoeysCross
  ( ZoeysCross(..)
  , zoeysCross
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetCard ZoeysCross
zoeysCross = accessory ZoeysCross Cards.zoeysCross

instance HasAbilities env ZoeysCross where
  getAbilities _ _ (ZoeysCross x) = pure
    [ restrictedAbility x 1 OwnsThis
      $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy)
      $ Costs [ExhaustCost (toTarget x), ResourceCost 1]
    ]

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source [Window _ (Window.EnemyEngaged _ eid)] 1 _
      | isSource attrs source -> a <$ push (EnemyDamage eid iid source 1)
    _ -> ZoeysCross <$> runMessage msg attrs
