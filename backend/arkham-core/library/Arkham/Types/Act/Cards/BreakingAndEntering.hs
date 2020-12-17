{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Act.Cards.BreakingAndEntering
  ( BreakingAndEntering(..)
  , breakingAndEntering
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Card.EncounterCardMatcher

newtype BreakingAndEntering = BreakingAndEntering Attrs
  deriving newtype (Show, ToJSON, FromJSON)

breakingAndEntering :: BreakingAndEntering
breakingAndEntering = BreakingAndEntering
  $ baseAttrs "02124" "Breaking and Entering" (Act 2 A) Nothing

instance ActionRunner env => HasActions env BreakingAndEntering where
  getActions i window (BreakingAndEntering x) = getActions i window x

instance ActRunner env => RunMessage env BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs@Attrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure $ BreakingAndEntering $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      mHuntingHorror <- fmap unStoryEnemyId <$> getId (CardCode "02141")
      case mHuntingHorror of
        Just eid -> do
          lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
            <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
          a <$ unshiftMessages
            [ chooseOne
              leadInvestigatorId
              [ TargetLabel
                  (InvestigatorTarget iid)
                  [AddCampaignCardToDeck iid "02138"]
              | iid <- investigatorIds
              ]
            , EnemySpawn Nothing lid eid
            , Ready (EnemyTarget eid)
            , NextAct actId "02125"
            ]
        Nothing -> a <$ unshiftMessages
          [ chooseOne
            leadInvestigatorId
            [ TargetLabel
                (InvestigatorTarget iid)
                [AddCampaignCardToDeck iid "02138"]
            | iid <- investigatorIds
            ]
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (EncounterCardMatchByCardCode "02141")
          ]
    FoundEnemyInVoid _ eid -> do
      lid <- fromJustNote "Exhibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages [EnemySpawn Nothing lid eid, NextAct actId "02125"]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      lid <- fromJustNote "Exibit Hall (Restricted Hall) missing"
        <$> getId (LocationWithFullTitle "Exhibit Hall" "Restricted Hall")
      a <$ unshiftMessages
        [SpawnEnemyAt (EncounterCard ec) lid, NextAct actId "02125"]
    WhenEnterLocation _ "02137" ->
      a <$ unshiftMessage (AdvanceAct actId (toSource attrs))
    _ -> BreakingAndEntering <$> runMessage msg attrs
