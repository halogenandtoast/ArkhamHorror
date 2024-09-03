module Arkham.Skill.Cards.SurvivalInstinct (
  survivalInstinct,
  SurvivalInstinct (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Game.Helpers
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
      engagedEnemies <- select $ enemyEngagedWith iid
      targets <- getConnectedMoveLocations iid (toSource attrs)
      canDisengage <- iid <=~> InvestigatorCanDisengage
      player <- getPlayer iid
      let
        moveOptions =
          chooseOrRunOne player
            $ [Label "Do not move to a connecting location" []]
            <> targetLabels targets (only . Move . move attrs iid)

      case engagedEnemies of
        es | notNull es && canDisengage -> do
          pushAll
            $ [ chooseOne
                  player
                  [ Label "Disengage from each other enemy" $ map (DisengageEnemy iid) es
                  , Label "Skip" []
                  ]
              ]
            <> [moveOptions | notNull targets]
        _ -> unless (null targets) $ push moveOptions
      pure s
    _ -> SurvivalInstinct <$> runMessage msg attrs
