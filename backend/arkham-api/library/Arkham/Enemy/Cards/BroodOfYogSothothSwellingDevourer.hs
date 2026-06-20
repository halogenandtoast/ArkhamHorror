module Arkham.Enemy.Cards.BroodOfYogSothothSwellingDevourer (broodOfYogSothothSwellingDevourer) where

import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype BroodOfYogSothothSwellingDevourer = BroodOfYogSothothSwellingDevourer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothothSwellingDevourer :: EnemyCard BroodOfYogSothothSwellingDevourer
broodOfYogSothothSwellingDevourer =
  enemy
    BroodOfYogSothothSwellingDevourer
    Cards.broodOfYogSothothSwellingDevourer

instance HasModifiersFor BroodOfYogSothothSwellingDevourer where
  getModifiersFor (BroodOfYogSothothSwellingDevourer a) = do
    healthModifier <- perPlayer 2
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      , CannotBeDamagedByPlayerSourcesExcept (SourceIsAsset (AssetIs Assets.esotericFormula.cardCode))
      ]

instance RunMessage BroodOfYogSothothSwellingDevourer where
  runMessage msg (BroodOfYogSothothSwellingDevourer attrs) = runQueueT $ case msg of
    _ -> BroodOfYogSothothSwellingDevourer <$> liftRunMessage msg attrs
