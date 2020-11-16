{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.DarkYoungHost where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype DarkYoungHost = DarkYoungHost Attrs
  deriving newtype (Show, ToJSON, FromJSON)

darkYoungHost :: EnemyId -> DarkYoungHost
darkYoungHost uuid =
  DarkYoungHost
    $ baseAttrs uuid "81033"
    $ (healthDamage .~ 2)
    . (sanityDamage .~ 1)
    . (fight .~ 4)
    . (health .~ Static 5)
    . (evade .~ 2)

instance HasModifiersFor env DarkYoungHost where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DarkYoungHost where
  getActions i window (DarkYoungHost attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env DarkYoungHost where
  runMessage msg e@(DarkYoungHost attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy _ _ eid | eid == enemyId -> do
      leadInvestigatorId <- getLeadInvestigatorId
      bayouLocations <- asks $ setToList . getSet [Bayou]
      e <$ spawnAtOneOf leadInvestigatorId enemyId bayouLocations
    PlaceClues (LocationTarget lid) n | lid == enemyLocation -> do
      unshiftMessage $ RemoveClues (LocationTarget lid) n
      pure . DarkYoungHost $ attrs & clues +~ n
    When (EnemyDefeated eid _ _ _ _ _) | eid == enemyId ->
      e <$ unshiftMessage (PlaceClues (LocationTarget enemyLocation) enemyClues)
    _ -> DarkYoungHost <$> runMessage msg attrs
