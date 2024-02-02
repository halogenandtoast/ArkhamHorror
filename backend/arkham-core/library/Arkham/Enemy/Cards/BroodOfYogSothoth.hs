module Arkham.Enemy.Cards.BroodOfYogSothoth (
  BroodOfYogSothoth (..),
  broodOfYogSothoth,
) where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Message qualified as Msg
import Arkham.Name
import Arkham.Projection

newtype BroodOfYogSothoth = BroodOfYogSothoth EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

broodOfYogSothoth :: EnemyCard BroodOfYogSothoth
broodOfYogSothoth =
  enemy BroodOfYogSothoth Cards.broodOfYogSothoth (6, Static 1, 3) (2, 2)

instance HasModifiersFor BroodOfYogSothoth where
  getModifiersFor target (BroodOfYogSothoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 1)
    pure
      $ toModifiers
        a
        [ HealthModifier healthModifier
        , CanOnlyBeAttackedByAbilityOn (singleton $ CardCode "02219")
        ]
  getModifiersFor _ _ = pure []

instance RunMessage BroodOfYogSothoth where
  runMessage msg e@(BroodOfYogSothoth attrs) = case msg of
    Msg.EnemyDamage eid (damageAssignmentSource -> AssetSource aid)
      | eid == enemyId attrs -> do
          name <- field AssetName aid
          if name == mkName "Esoteric Formula"
            then BroodOfYogSothoth <$> runMessage msg attrs
            else pure e
    Msg.EnemyDamage eid _ | eid == enemyId attrs -> pure e
    _ -> BroodOfYogSothoth <$> runMessage msg attrs
