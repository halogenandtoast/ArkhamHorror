module Arkham.Enemy.Cards.BroodOfYogSothothThrashingSpawn (broodOfYogSothothThrashingSpawn) where

import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype BroodOfYogSothothThrashingSpawn = BroodOfYogSothothThrashingSpawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothothThrashingSpawn :: EnemyCard BroodOfYogSothothThrashingSpawn
broodOfYogSothothThrashingSpawn =
  enemy BroodOfYogSothothThrashingSpawn Cards.broodOfYogSothothThrashingSpawn

instance HasModifiersFor BroodOfYogSothothThrashingSpawn where
  getModifiersFor (BroodOfYogSothothThrashingSpawn a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      , CannotBeDamagedByPlayerSourcesExcept (SourceIsAsset (AssetIs Assets.esotericFormula.cardCode))
      ]

instance RunMessage BroodOfYogSothothThrashingSpawn where
  runMessage msg (BroodOfYogSothothThrashingSpawn attrs) = runQueueT $ case msg of
    _ -> BroodOfYogSothothThrashingSpawn <$> liftRunMessage msg attrs
