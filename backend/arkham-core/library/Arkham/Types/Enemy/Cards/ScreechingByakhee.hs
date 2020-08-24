{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.ScreechingByakhee where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype ScreechingByakhee = ScreechingByakhee Attrs
  deriving newtype (Show, ToJSON, FromJSON)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid = ScreechingByakhee $ (baseAttrs uuid "01175")
  { enemyHealthDamage = 1
  , enemySanityDamage = 2
  , enemyFight = 3
  , enemyHealth = Static 4
  , enemyEvade = 3
  , enemyPrey = LowestRemainingSanity
  }

instance (IsInvestigator investigator) => HasActions env investigator ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg e@(ScreechingByakhee attrs@Attrs {..}) = case msg of
    PostPlayerWindow -> do
      sanities <- map unRemainingSanity <$> traverse
        (asks . getCount)
        (HashSet.toList enemyEngagedInvestigators)
      when
        (any (<= 4) sanities)
        (unshiftMessages
          [ AddModifier
            (EnemyTarget enemyId)
            (EnemyFight 1 (EnemySource enemyId))
          , AddModifier
            (EnemyTarget enemyId)
            (EnemyEvade 1 (EnemySource enemyId))
          ]
        )
      e <$ unshiftMessage
        (RemoveAllModifiersOnTargetFrom
          (EnemyTarget enemyId)
          (EnemySource enemyId)
        )
      -- ^ we unshift this last so we do not double up
    _ -> ScreechingByakhee <$> runMessage msg attrs

-- EnemyEvaded
-- EnemyEngageInvestigator
-- EngageEnemy
-- InvestigatorEliminated
-- UnengageEnemy
