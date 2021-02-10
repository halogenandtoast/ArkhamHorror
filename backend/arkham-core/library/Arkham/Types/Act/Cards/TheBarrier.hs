module Arkham.Types.Act.Cards.TheBarrier where

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


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype TheBarrier = TheBarrier ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrier :: TheBarrier
theBarrier = TheBarrier $ baseAttrs "01109" "The Barrier" (Act 2 A) Nothing

instance ActionRunner env => HasActions env TheBarrier where
  getActions i window (TheBarrier x) = getActions i window x

instance ActRunner env => RunMessage env TheBarrier where
  runMessage msg a@(TheBarrier attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      hallwayId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Hallway"
      investigatorIds <- getSetList hallwayId
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid $ toSource attrs]
          | iid <- investigatorIds
          ]
        )
      pure $ TheBarrier $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      hallwayId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "Hallway"
      ghoulPriest <- EncounterCard <$> genEncounterCard "01116"
      litaChantler <- PlayerCard <$> genPlayerCard "01117"
      a <$ unshiftMessages
        [ RevealLocation Nothing "01115"
        , CreateStoryAssetAt litaChantler "01115"
        , CreateEnemyAt ghoulPriest hallwayId
        , NextAct aid "01110"
        ]
    EndRoundWindow -> do
      investigatorIds <- getSetList @InvestigatorId
        (LocationWithTitle "Hallway")
      leadInvestigatorId <- getLeadInvestigatorId
      totalSpendableClueCount <- getSpendableClueCount investigatorIds
      requiredClueCount <- getPlayerCountValue (PerPlayer 3)
      if totalSpendableClueCount >= requiredClueCount
        then a <$ unshiftMessage
          (chooseOne
            leadInvestigatorId
            [ AdvanceAct actId (toSource attrs)
            , Continue "Continue without advancing act"
            ]
          )
        else pure a
    _ -> TheBarrier <$> runMessage msg attrs
