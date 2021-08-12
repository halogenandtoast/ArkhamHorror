module Arkham.Types.Skill.Cards.SurvivalInstinct
  ( survivalInstinct
  , SurvivalInstinct(..)
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

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving anyclass IsSkill
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: SkillCard SurvivalInstinct
survivalInstinct = skill SurvivalInstinct Cards.survivalInstinct

instance HasModifiersFor env SurvivalInstinct
instance HasActions SurvivalInstinct

instance SkillRunner env => RunMessage env SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs@SkillAttrs {..}) = case msg of
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
                   "Disengage from each other enemy"
                   [ DisengageEnemy iid eid | eid <- es ]
                 , Label "Skip" []
                 ]
             ]
            <> [ moveOptions | notNull unblockedConnectedLocationIds ]
            )
    _ -> SurvivalInstinct <$> runMessage msg attrs
