{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Cards.SurvivalInstinct where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Target
import qualified Data.HashSet as HashSet

newtype SurvivalInstinct = SurvivalInstinct Attrs
  deriving newtype (Show, ToJSON, FromJSON)

survivalInstinct :: InvestigatorId -> SkillId -> SurvivalInstinct
survivalInstinct iid uuid = SurvivalInstinct $ baseAttrs iid uuid "01081"

instance HasActions env investigator SurvivalInstinct where
  getActions i window (SurvivalInstinct attrs) = getActions i window attrs

instance (SkillRunner env) => RunMessage env SurvivalInstinct where
  runMessage msg s@(SurvivalInstinct attrs@Attrs {..}) = case msg of
    PassedSkillTest iid (Just Evade) _ (SkillTarget sid) _ | sid == skillId ->
      do
        engagedEnemyIds <- HashSet.toList <$> asks (getSet iid)
        locationId <- asks (getId @LocationId iid)
        blockedLocationIds <- HashSet.map unBlockedLocationId
          <$> asks (getSet ())
        connectedLocationIds <- HashSet.map unConnectedLocationId
          <$> asks (getSet locationId)
        let
          unblockedConnectedLocationIds =
            HashSet.toList
              $ connectedLocationIds
              `difference` blockedLocationIds
          moveOptions = Ask
            iid
            (ChooseOne
              ([Label "Do not move to a connecting location" []]
              <> [ MoveAction iid lid False
                 | lid <- unblockedConnectedLocationIds
                 ]
              )
            )

        s <$ case engagedEnemyIds of
          [] -> if null unblockedConnectedLocationIds
            then pure ()
            else unshiftMessage moveOptions
          es -> unshiftMessages
            ([ Ask iid $ ChooseOne
                 [ Label
                   "Disengage from each other enemy"
                   [ DisengageEnemy iid eid | eid <- es ]
                 , Label "Skip" []
                 ]
             ]
            <> [ moveOptions | not (null unblockedConnectedLocationIds) ]
            )
    _ -> SurvivalInstinct <$> runMessage msg attrs
