module Arkham.Types.Enemy.Cards.BroodOfYogSothoth
  ( BroodOfYogSothoth(..)
  , broodOfYogSothoth
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Source

newtype BroodOfYogSothoth = BroodOfYogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions)

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

instance (EnemyRunner env, HasName env AssetId) => RunMessage env BroodOfYogSothoth where
  runMessage msg e@(BroodOfYogSothoth attrs) = case msg of
    EnemyDamage eid _ (AssetSource aid) _ | eid == enemyId attrs -> do
      name <- getName aid
      if name == mkName "Esoteric Formula"
        then BroodOfYogSothoth <$> runMessage msg attrs
        else pure e
    EnemyDamage eid _ _ _ | eid == enemyId attrs -> pure e
    _ -> BroodOfYogSothoth <$> runMessage msg attrs
