module Arkham.Asset.Assets.ArmoredCar (armoredCar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Location (getConnectedLocations, withLocationOf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)
import Arkham.Placement (Placement (AtLocation))
import Arkham.Projection (field)
import Arkham.ScenarioLogKey (ScenarioLogKey (TheCarReachedItsTarget))
import Arkham.Story.Cards qualified as Stories

newtype ArmoredCar = ArmoredCar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

armoredCar :: AssetCard ArmoredCar
armoredCar = asset ArmoredCar Cards.armoredCar

instance HasAbilities ArmoredCar where
  getAbilities (ArmoredCar a) =
    [ restricted a 1 (youExist $ InvestigatorAt $ locationWithAsset a) actionAbility
    , mkAbility a 2 $ forced $ PhaseBegins #when #enemy
    ]

instance RunMessage ArmoredCar where
  runMessage msg a@(ArmoredCar attrs) = runQueueT $ case msg of
    PlaceAsset aid (AtLocation lid) | aid == attrs.id -> do
      whenM (lid `matches` LocationWithTitle "Fungus Mound") do
        remember TheCarReachedItsTarget
        lead <- getLead
        selectEach (storyIs Stories.escortTheCar) $ push . Flip lead GameSource . StoryTarget
      ArmoredCar <$> liftRunMessage msg attrs
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \loc -> do
        enemies <- select $ ReadyEnemy <> EnemyAt (LocationWithId loc)
        for_ enemies \enemy -> do
          damage <- field EnemyDamage enemy
          isDestroyer <- enemy `matches` enemyIs Enemies.miGoDestroyer
          dealAssetDamage attrs.id (EnemyAttackSource enemy) (damage + if isDestroyer then 2 else 0)
          exhaustEnemy (attrs.ability 2) enemy
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      investigatorCount <- getPlayerCount
      doStep (max 0 $ investigatorCount - 1) msg
      pure a
    DoStep remaining original@(UseThisAbility iid (isSource attrs -> True) 1) | remaining > 0 -> do
      withLocationOf attrs \loc -> do
        payers <- select $ InvestigatorAt (LocationWithId loc) <> InvestigatorWithAnyActionsRemaining
        chooseOrRunOneM iid $ portraits payers \payer -> do
          spendActions payer (attrs.ability 1) 1
          doStep (remaining - 1) original
      pure a
    DoStep 0 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      withLocationOf attrs \loc -> do
        connected <- getConnectedLocations loc
        chooseOrRunOneM iid $ targets connected $ push . PlaceAsset attrs.id . AtLocation
      pure a
    _ -> ArmoredCar <$> liftRunMessage msg attrs
