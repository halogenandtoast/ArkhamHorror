module Arkham.Asset.Assets.BearTrap (BearTrap (..), bearTrap) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype BearTrap = BearTrap AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearTrap :: AssetCard BearTrap
bearTrap = assetWith BearTrap Cards.bearTrap (isStoryL .~ True)

instance HasModifiersFor BearTrap where
  getModifiersFor (BearTrap a) = case a.placement of
    AttachedToEnemy eid -> modified_ a eid [EnemyFight (-1), EnemyEvade (-1)]
    _ -> pure mempty

instance HasAbilities BearTrap where
  getAbilities (BearTrap x) =
    [ restricted x 1 restriction $ FastAbility Free
    , mkAbility x 2 $ forced $ EnemyEnters #after LocationOfThis (enemyIs Cards.theRougarou)
    ]
   where
    restriction = case x.placement of
      AttachedToEnemy _ -> Never
      _ -> ControlsThis

instance RunMessage BearTrap where
  runMessage msg a@(BearTrap attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationId <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      push $ AttachAsset attrs.id (LocationTarget locationId)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 [(windowType -> Window.EnemyEnters eid _)] _ -> do
      push $ AttachAsset attrs.id (EnemyTarget eid)
      pure a
    _ -> BearTrap <$> runMessage msg attrs
