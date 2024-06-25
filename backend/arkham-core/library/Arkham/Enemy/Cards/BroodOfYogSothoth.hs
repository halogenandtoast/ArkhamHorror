module Arkham.Enemy.Cards.BroodOfYogSothoth (BroodOfYogSothoth (..), broodOfYogSothoth) where

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype BroodOfYogSothoth = BroodOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothoth :: EnemyCard BroodOfYogSothoth
broodOfYogSothoth = enemy BroodOfYogSothoth Cards.broodOfYogSothoth (6, Static 1, 3) (2, 2)

instance HasModifiersFor BroodOfYogSothoth where
  getModifiersFor target (BroodOfYogSothoth a) | isTarget a target = do
    healthModifier <- perPlayer 1
    modified a [HealthModifier healthModifier, CanOnlyBeAttackedByAbilityOn (singleton "02219")]
  getModifiersFor _ _ = pure []

instance RunMessage BroodOfYogSothoth where
  runMessage msg e@(BroodOfYogSothoth attrs) = case msg of
    Msg.EnemyDamage eid (damageAssignmentSource -> (.asset) -> Just aid) | eid == enemyId attrs -> do
      isEsotericFormula <- aid <=~> AssetWithTitle "Esoteric Formula"
      if isEsotericFormula
        then BroodOfYogSothoth <$> runMessage msg attrs
        else pure e
    Msg.EnemyDamage eid _ | eid == enemyId attrs -> pure e
    _ -> BroodOfYogSothoth <$> runMessage msg attrs
