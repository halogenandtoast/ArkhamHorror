module Arkham.Types.Skill.Cards.SurvivalInstinct where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Action
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype SurvivalInstinct = SurvivalInstinct SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalInstinct :: InvestigatorId -> SkillId -> SurvivalInstinct
survivalInstinct iid uuid = SurvivalInstinct $ baseAttrs iid uuid "01081"

instance HasModifiersFor env SurvivalInstinct where
  getModifiersFor = noModifiersFor

instance HasActions env SurvivalInstinct where
  getActions i window (SurvivalInstinct attrs) = getActions i window attrs

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
            <> [ MoveAction iid lid False
               | lid <- unblockedConnectedLocationIds
               ]
            )

        s <$ case engagedEnemyIds of
          [] -> if null unblockedConnectedLocationIds
            then pure ()
            else unshiftMessage moveOptions
          es -> unshiftMessages
            ([ chooseOne
                 iid
                 [ Label
                   "Disengage from each other enemy"
                   [ DisengageEnemy iid eid | eid <- es ]
                 , Label "Skip" []
                 ]
             ]
            <> [ moveOptions | not (null unblockedConnectedLocationIds) ]
            )
    _ -> SurvivalInstinct <$> runMessage msg attrs
