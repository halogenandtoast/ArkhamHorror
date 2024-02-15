module Arkham.Skill.Cards.SurvivalInstinct2 (
  survivalInstinct2,
  SurvivalInstinct2 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message
import Arkham.Movement
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype SurvivalInstinct2 = SurvivalInstinct2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct2 :: SkillCard SurvivalInstinct2
survivalInstinct2 = skill SurvivalInstinct2 Cards.survivalInstinct2

instance RunMessage SurvivalInstinct2 where
  runMessage msg s@(SurvivalInstinct2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (SkillTarget sid) _ _ | sid == skillId -> do
      engagedEnemyIds <- select EnemyEngagedWithYou
      unblockedConnectedLocationIds <- getAccessibleLocations iid attrs
      player <- getPlayer iid
      let
        moveOptions =
          chooseOrRunOne player
            $ [Label "Do not move to a connecting location" []]
            <> [ targetLabel lid [Move $ move attrs iid lid]
               | lid <- unblockedConnectedLocationIds
               ]

      case engagedEnemyIds of
        [] -> push moveOptions
        es ->
          pushAll
            $ [ chooseOne
                  player
                  [ Label
                      "Evade each other enemy"
                      [EnemyEvaded iid eid | eid <- es]
                  , Label "Skip" []
                  ]
              ]
            <> [moveOptions | notNull unblockedConnectedLocationIds]
      pure s
    _ -> SurvivalInstinct2 <$> runMessage msg attrs
