{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.MarshGug where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype MarshGug = MarshGug Attrs
  deriving newtype (Show, ToJSON, FromJSON)

marshGug :: EnemyId -> MarshGug
marshGug uuid = MarshGug $ (baseAttrs uuid "81032")
  { enemyHealthDamage = 2
  , enemySanityDamage = 1
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 3
  }

instance HasModifiersFor env MarshGug where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env MarshGug where
  getModifiers _ (MarshGug Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env MarshGug where
  getActions i window (MarshGug attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MarshGug where
  runMessage msg e@(MarshGug attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- asks $ setToList . getSet [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    _ -> MarshGug <$> runMessage msg attrs
