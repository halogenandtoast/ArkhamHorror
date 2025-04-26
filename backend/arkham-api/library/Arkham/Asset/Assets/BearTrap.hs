module Arkham.Asset.Assets.BearTrap (bearTrap) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Matcher
import Arkham.Placement

newtype BearTrap = BearTrap AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bearTrap :: AssetCard BearTrap
bearTrap = asset BearTrap Cards.bearTrap

instance HasModifiersFor BearTrap where
  getModifiersFor (BearTrap a) = case a.placement of
    AttachedToEnemy eid -> modified_ a eid [EnemyFight (-1), EnemyEvade (-1)]
    _ -> pure ()

instance HasAbilities BearTrap where
  getAbilities (BearTrap x) =
    [ restricted x 1 restriction $ FastAbility Free
    , mkAbility x 2
        $ forced
        $ EnemyEnters #after (LocationWithAttachedAsset $ be x) (enemyIs Cards.theRougarou)
    ]
   where
    restriction = case x.placement of
      AttachedToEnemy _ -> Never
      _ -> ControlsThis

instance RunMessage BearTrap where
  runMessage msg a@(BearTrap attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid (attach attrs)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (enteringEnemy -> eid) _ -> do
      attach attrs eid
      pure a
    _ -> BearTrap <$> liftRunMessage msg attrs
