module Arkham.Enemy.Cards.BroodOfYogSothoth
  ( BroodOfYogSothoth(..)
  , broodOfYogSothoth
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Message
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.Source

newtype BroodOfYogSothoth = BroodOfYogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

broodOfYogSothoth :: EnemyCard BroodOfYogSothoth
broodOfYogSothoth =
  enemy BroodOfYogSothoth Cards.broodOfYogSothoth (6, Static 1, 3) (2, 2)

instance HasCount PlayerCount env () => HasModifiersFor env BroodOfYogSothoth where
  getModifiersFor _ target (BroodOfYogSothoth a) | isTarget a target = do
    healthModifier <- getPlayerCountValue (PerPlayer 1)
    pure $ toModifiers
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn (singleton $ CardCode "02219")
      ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env BroodOfYogSothoth where
  runMessage msg e@(BroodOfYogSothoth attrs) = case msg of
    EnemyDamage eid _ (AssetSource aid) _ _ | eid == enemyId attrs -> do
      name <- getName aid
      if name == mkName "Esoteric Formula"
        then BroodOfYogSothoth <$> runMessage msg attrs
        else pure e
    EnemyDamage eid _ _ _ _ | eid == enemyId attrs -> pure e
    _ -> BroodOfYogSothoth <$> runMessage msg attrs
