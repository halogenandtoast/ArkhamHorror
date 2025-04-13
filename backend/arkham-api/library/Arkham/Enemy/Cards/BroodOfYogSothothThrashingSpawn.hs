module Arkham.Enemy.Cards.BroodOfYogSothothThrashingSpawn (broodOfYogSothothThrashingSpawn) where

import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype BroodOfYogSothothThrashingSpawn = BroodOfYogSothothThrashingSpawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothothThrashingSpawn :: EnemyCard BroodOfYogSothothThrashingSpawn
broodOfYogSothothThrashingSpawn =
  enemy BroodOfYogSothothThrashingSpawn Cards.broodOfYogSothothThrashingSpawn (7, Static 1, 3) (2, 1)

instance HasModifiersFor BroodOfYogSothothThrashingSpawn where
  getModifiersFor (BroodOfYogSothothThrashingSpawn a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      ]

instance RunMessage BroodOfYogSothothThrashingSpawn where
  runMessage msg (BroodOfYogSothothThrashingSpawn attrs) = runQueueT $ case msg of
    _ -> BroodOfYogSothothThrashingSpawn <$> liftRunMessage msg attrs
