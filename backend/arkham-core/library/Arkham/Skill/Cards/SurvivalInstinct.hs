module Arkham.Skill.Cards.SurvivalInstinct (
  survivalInstinct,
  SurvivalInstinct (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance RunMessage SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (SkillTarget sid) _ _ | sid == toId attrs -> do
      engagedEnemyIds <- selectList $ enemyEngagedWith iid
      unblockedConnectedLocationIds <- selectList AccessibleLocation
      canMove <- iid <=~> InvestigatorCanMove
      canDisengage <- iid <=~> InvestigatorCanDisengage
      let
        moveOptions =
          chooseOrRunOne iid
            $ [Label "Do not move to a connecting location" []]
              <> [ targetLabel lid [Move $ move attrs iid lid]
                 | lid <- unblockedConnectedLocationIds
                 ]

      case engagedEnemyIds of
        es | notNull es && canDisengage -> do
          pushAll
            $ [ chooseOne
                  iid
                  [ Label "Disengage from each other enemy"
                      $ [DisengageEnemy iid eid | eid <- es]
                  , Label "Skip" []
                  ]
              ]
              <> [ moveOptions
                 | notNull unblockedConnectedLocationIds && canMove
                 ]
        _ -> unless (null unblockedConnectedLocationIds) $ push moveOptions
      pure s
    _ -> SurvivalInstinct <$> runMessage msg attrs
