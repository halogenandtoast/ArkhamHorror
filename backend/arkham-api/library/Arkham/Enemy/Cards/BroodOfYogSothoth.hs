module Arkham.Enemy.Cards.BroodOfYogSothoth (BroodOfYogSothoth (..), broodOfYogSothoth) where

import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype BroodOfYogSothoth = BroodOfYogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothoth :: EnemyCard BroodOfYogSothoth
broodOfYogSothoth = enemy BroodOfYogSothoth Cards.broodOfYogSothoth (6, Static 1, 3) (2, 2)

instance HasModifiersFor BroodOfYogSothoth where
  getModifiersFor (BroodOfYogSothoth a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton $ Assets.esotericFormula.cardCode
      ]

instance RunMessage BroodOfYogSothoth where
  runMessage msg e@(BroodOfYogSothoth attrs) = runQueueT $ case msg of
    Msg.EnemyDamage eid (damageAssignmentSource -> (.asset) -> Just aid) | eid == enemyId attrs -> do
      isEsotericFormula <- aid <=~> AssetWithTitle "Esoteric Formula"
      if isEsotericFormula
        then BroodOfYogSothoth <$> liftRunMessage msg attrs
        else pure e
    Msg.EnemyDamage eid _ | eid == enemyId attrs -> pure e
    _ -> BroodOfYogSothoth <$> liftRunMessage msg attrs
