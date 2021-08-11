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
import Arkham.Types.Message
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype ZoeysCross = ZoeysCross AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zoeysCross :: AssetCard ZoeysCross
zoeysCross = accessory ZoeysCross Cards.zoeysCross

instance HasModifiersFor env ZoeysCross

instance HasActions ZoeysCross where
  getActions (ZoeysCross x) =
    [ restrictedAbility x 1 OwnsThis
        $ ReactionAbility (EnemyEngaged Timing.After You AnyEnemy)
        $ Costs [ExhaustThis, ResourceCost 1]
    ]

instance (AssetRunner env) => RunMessage env ZoeysCross where
  runMessage msg a@(ZoeysCross attrs) = case msg of
    UseCardAbility iid source [Window _ (Window.EnemyEngageInvestigator _ eid)] 1 _
      | isSource attrs source
      -> a <$ push (EnemyDamage eid iid source 1)
    _ -> ZoeysCross <$> runMessage msg attrs
