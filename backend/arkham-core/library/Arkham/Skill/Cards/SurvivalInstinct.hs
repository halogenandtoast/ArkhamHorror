module Arkham.Skill.Cards.SurvivalInstinct
  ( survivalInstinct
  , SurvivalInstinct(..)
  ) where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Game.Helpers
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Message
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance RunMessage SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Evade) _ (SkillTarget sid) _ _ | sid == skillId ->
      do
        engagedEnemyIds <- selectList EnemyEngagedWithYou
        unblockedConnectedLocationIds <- selectList AccessibleLocation
        canMove <- iid <=~> InvestigatorCanMove
        canDisengage <- iid <=~> InvestigatorCanDisengage
        let
          moveOptions =
            chooseOne iid
              $ [Label "Do not move to a connecting location" []]
              <> [ targetLabel lid [MoveAction iid lid Free False]
                 | lid <- unblockedConnectedLocationIds
                 ]

        case engagedEnemyIds of
          es | notNull es && canDisengage ->
            pushAll
              $ [ chooseOne
                    iid
                    [ Label
                      "Disengage from each other enemy"
                      [ DisengageEnemy iid eid | eid <- es ]
                    , Label "Skip" []
                    ]
                ]
              <> [ moveOptions
                 | notNull unblockedConnectedLocationIds && canMove
                 ]
          _ -> unless (null unblockedConnectedLocationIds) $ push moveOptions
        pure s
    _ -> SurvivalInstinct <$> runMessage msg attrs
