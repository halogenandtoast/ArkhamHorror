module Arkham.Asset.Assets.CashCart (cashCart) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Location (getConnectedLocations, withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.ScenarioLogKey

newtype CashCart = CashCart AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cashCart :: AssetCard CashCart
cashCart = assetWith CashCart Cards.cashCart (healthL ?~ 3)

instance HasAbilities CashCart where
  getAbilities (CashCart a) =
    [ restricted a 1 OnSameLocation $ FastAbility Free
    , restricted a 2 OnSameLocation
        $ triggered
          (oneOf [EnemyEnters #when YourLocation AnyEnemy, EnemyWouldEngage #when You AnyEnemy])
          (DamageCost (a.ability 2) (toTarget a) 1)
    , mkAbility a 3 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage CashCart where
  runMessage msg a@(CashCart attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> do
        connected <- getConnectedLocations loc
        chooseTargetM iid connected $ place attrs . AtLocation
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      withLocationOf attrs \loc -> do
        selectEach (enemyAt loc) \enemy -> do
          roundModifier (attrs.ability 2) enemy (CannotEngage iid)
      pure a
    UseCardAbility _ (isSource attrs -> True) 3 ws _ -> do
      cancelWindowBatch ws
      remember StayedOutOfSight
      removeFromGame attrs
      pure a
    _ -> CashCart <$> liftRunMessage msg attrs
