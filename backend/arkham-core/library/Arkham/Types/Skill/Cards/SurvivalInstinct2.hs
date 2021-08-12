module Arkham.Types.Skill.Cards.SurvivalInstinct2
  ( survivalInstinct2
  , SurvivalInstinct2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.Target

newtype SurvivalInstinct2 = SurvivalInstinct2 SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct2 :: SkillCard SurvivalInstinct2
survivalInstinct2 = skill SurvivalInstinct2 Cards.survivalInstinct2

instance HasModifiersFor env SurvivalInstinct2
instance HasActions SurvivalInstinct2

instance SkillRunner env => RunMessage env SurvivalInstinct2 where
  runMessage msg s@(SurvivalInstinct2 attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid (Just Evade) _ (SkillTarget sid) _ _ | sid == skillId ->
      do
        engagedEnemyIds <- getSetList iid
        locationId <- getId @LocationId iid
        blockedLocationIds <- mapSet unBlockedLocationId <$> getSet ()
        connectedLocationIds <- mapSet unConnectedLocationId
          <$> getSet locationId
        let
          unblockedConnectedLocationIds =
            setToList $ connectedLocationIds `difference` blockedLocationIds
          moveOptions = chooseOne
            iid
            ([Label "Do not move to a connecting location" []]
            <> [ MoveAction iid lid Free False
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
