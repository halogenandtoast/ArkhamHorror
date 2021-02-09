module Arkham.Types.Skill.Cards.SurvivalInstinct where


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
