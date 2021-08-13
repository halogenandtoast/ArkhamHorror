module Arkham.Types.Enemy.Cards.CloverClubPitBoss
  ( CloverClubPitBoss(..)
  , cloverClubPitBoss
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import qualified Arkham.Types.Timing as Timing

newtype CloverClubPitBoss = CloverClubPitBoss EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubPitBoss :: EnemyCard CloverClubPitBoss
cloverClubPitBoss = enemyWith
  CloverClubPitBoss
  Cards.cloverClubPitBoss
  (3, Static 4, 3)
  (2, 0)
  (preyL .~ HighestSkill SkillIntellect)

instance HasActions CloverClubPitBoss where
  getActions (CloverClubPitBoss x) =
    [ mkAbility x 1 $ ForcedAbility $ GainsClues
        Timing.After
        (InvestigatorAt SameLocation)
        AnyValue
    ]

instance EnemyRunner env => RunMessage env CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ pushAll
      ([ Ready (toTarget attrs) | enemyExhausted ]
      <> [ EnemyEngageInvestigator enemyId iid
         , EnemyAttackIfEngaged enemyId (Just iid)
         ]
      )
    _ -> CloverClubPitBoss <$> runMessage msg attrs
