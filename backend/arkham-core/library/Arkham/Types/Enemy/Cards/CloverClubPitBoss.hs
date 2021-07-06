module Arkham.Types.Enemy.Cards.CloverClubPitBoss
  ( CloverClubPitBoss(..)
  , cloverClubPitBoss
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType

newtype CloverClubPitBoss = CloverClubPitBoss EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubPitBoss :: EnemyCard CloverClubPitBoss
cloverClubPitBoss = enemyWith
  CloverClubPitBoss
  Cards.cloverClubPitBoss
  (3, Static 4, 3)
  (2, 0)
  (preyL .~ HighestSkill SkillIntellect)

instance HasModifiersFor env CloverClubPitBoss where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CloverClubPitBoss where
  getActions i window (CloverClubPitBoss attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs@EnemyAttrs {..}) = case msg of
    After (GainClues iid n) | n > 0 -> do
      lid <- getId iid
      e <$ when
        (lid == enemyLocation)
        (pushAll
        $ [ Ready (toTarget attrs) | enemyExhausted ]
        <> [ EnemyEngageInvestigator enemyId iid
           , EnemyAttackIfEngaged enemyId (Just iid)
           ]
        )
    _ -> CloverClubPitBoss <$> runMessage msg attrs
