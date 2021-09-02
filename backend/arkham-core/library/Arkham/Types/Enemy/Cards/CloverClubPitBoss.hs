module Arkham.Types.Enemy.Cards.CloverClubPitBoss
  ( CloverClubPitBoss(..)
  , cloverClubPitBoss
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
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

instance HasAbilities CloverClubPitBoss where
  getAbilities (CloverClubPitBoss x) = withBaseAbilities
    x
    [ restrictedAbility x 1 OnSameLocation $ ForcedAbility $ GainsClues
        Timing.After
        You
        AnyValue
    ]

instance EnemyRunner env => RunMessage env CloverClubPitBoss where
  runMessage msg e@(CloverClubPitBoss attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> e <$ pushAll
      ([ Ready (toTarget attrs) | enemyExhausted attrs ]
      <> [ EnemyEngageInvestigator (toId attrs) iid
         , EnemyAttackIfEngaged (toId attrs) (Just iid)
         ]
      )
    _ -> CloverClubPitBoss <$> runMessage msg attrs
