module Arkham.Skill.Cards.SurvivalInstinct2
  ( survivalInstinct2
  , SurvivalInstinct2(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Matcher hiding (MoveAction, EnemyEvaded)
import Arkham.Message
import Arkham.Skill.Runner
import Arkham.Target

newtype SurvivalInstinct2 = SurvivalInstinct2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct2 :: SkillCard SurvivalInstinct2
survivalInstinct2 = skill SurvivalInstinct2 Cards.survivalInstinct2

instance RunMessage SurvivalInstinct2 where
  runMessage msg s@(SurvivalInstinct2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Evade) _ (SkillTarget sid) _ _ | sid == skillId ->
      do
        engagedEnemyIds <- selectList EnemyEngagedWithYou
        unblockedConnectedLocationIds <- selectList AccessibleLocation
        let
          moveOptions = chooseOne
            iid
            ([Label "Do not move to a connecting location" []]
            <> [ targetLabel lid [MoveAction iid lid Free False]
               | lid <- unblockedConnectedLocationIds
               ]
            )

        s <$ case engagedEnemyIds of
          [] -> if null unblockedConnectedLocationIds
            then pure ()
            else push moveOptions
          es -> pushAll
            ([ chooseOne
                 iid
                 [ Label
                   "Evade each other enemy"
                   [ EnemyEvaded iid eid | eid <- es ]
                 , Label "Skip" []
                 ]
             ]
            <> [ moveOptions | notNull unblockedConnectedLocationIds ]
            )
    _ -> SurvivalInstinct2 <$> runMessage msg attrs
