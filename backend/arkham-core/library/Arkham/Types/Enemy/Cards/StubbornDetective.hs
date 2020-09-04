{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.StubbornDetective where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype StubbornDetective = StubbornDetective Attrs
  deriving newtype (Show, ToJSON, FromJSON)

stubbornDetective :: EnemyId -> StubbornDetective
stubbornDetective uuid = StubbornDetective $ (weaknessBaseAttrs uuid "01102")
  { enemyHealthDamage = 1
  , enemySanityDamage = 0
  , enemyFight = 3
  , enemyHealth = Static 2
  , enemyEvade = 2
  , enemyPrey = SetToBearer
  }

instance (IsInvestigator investigator) => HasActions env investigator StubbornDetective where
  getActions i window (StubbornDetective attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env StubbornDetective where
  runMessage msg e@(StubbornDetective attrs@Attrs {..}) = case msg of
    EnemySpawn _ eid | eid == enemyId -> runMessage PostPlayerWindow e
    EnemySpawnedAt _ eid | eid == enemyId -> runMessage PostPlayerWindow e
    EnemyMove eid _ _ | eid == enemyId -> runMessage PostPlayerWindow e
    MoveTo _ _ -> runMessage PostPlayerWindow e
    PrePlayerWindow -> runMessage PostPlayerWindow e
    PostPlayerWindow -> do
      investigatorIds <- HashSet.toList <$> asks (getSet ())
      locationInvestigatorIds <- HashSet.toList <$> asks (getSet enemyLocation)
      e <$ unshiftMessages
        ([ RemoveAllModifiersOnTargetFrom
             (InvestigatorTarget investigatorId)
             (EnemySource enemyId)
         | investigatorId <- investigatorIds
         ]
        <> [ AddModifier
               (InvestigatorTarget investigatorId)
               (EnemySource enemyId)
               Blank
           | investigatorId <- locationInvestigatorIds
           ]
        )
    _ -> StubbornDetective <$> runMessage msg attrs
