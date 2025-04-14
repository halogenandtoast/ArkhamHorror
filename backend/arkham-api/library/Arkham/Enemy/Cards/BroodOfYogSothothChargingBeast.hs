module Arkham.Enemy.Cards.BroodOfYogSothothChargingBeast (broodOfYogSothothChargingBeast) where

import Arkham.Ability
import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype BroodOfYogSothothChargingBeast = BroodOfYogSothothChargingBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broodOfYogSothothChargingBeast :: EnemyCard BroodOfYogSothothChargingBeast
broodOfYogSothothChargingBeast = enemy BroodOfYogSothothChargingBeast Cards.broodOfYogSothothChargingBeast (5, Static 1, 4) (2, 1)

instance HasModifiersFor BroodOfYogSothothChargingBeast where
  getModifiersFor (BroodOfYogSothothChargingBeast a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      ]

instance HasAbilities BroodOfYogSothothChargingBeast where
  getAbilities (BroodOfYogSothothChargingBeast a) =
    extend
      a
      [ restricted a 1 (criteria <> thisExists a UnengagedEnemy)
          $ forced
          $ EnemyMoves #after Anywhere (be a)
      ]
   where
    criteria = if toResultDefault (0 :: Int) a.meta == 1 then NoRestriction else Never

instance RunMessage BroodOfYogSothothChargingBeast where
  runMessage msg e@(BroodOfYogSothothChargingBeast attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ ChooseRandomLocation (toTarget attrs) mempty
      pure e
    EnemyMove eid _ | eid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      let n = toResultDefault (0 :: Int) attrs'.meta
      pure $ BroodOfYogSothothChargingBeast $ attrs' & setMeta (n + 1)
    EndRound -> do
      attrs' <- liftRunMessage msg attrs
      pure $ BroodOfYogSothothChargingBeast $ attrs' & setMeta (0 :: Int)
    _ -> BroodOfYogSothothChargingBeast <$> liftRunMessage msg attrs
