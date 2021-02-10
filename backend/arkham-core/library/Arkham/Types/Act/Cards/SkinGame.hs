module Arkham.Types.Act.Cards.SkinGame
  ( SkinGame(..)
  , skinGame
  ) where

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
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skinGame :: SkinGame
skinGame = SkinGame $ baseAttrs
  "02067"
  "Skin Game"
  (Act 2 A)
  (Just $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "VIP Area"))

instance ActionRunner env => HasActions env SkinGame where
  getActions i window (SkinGame x) = getActions i window x

instance ActRunner env => RunMessage env SkinGame where
  runMessage msg a@(SkinGame attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      vipAreaId <- fromJustNote "must exist"
        <$> getLocationIdWithTitle "VIP Area"
      investigatorIds <- getSetList vipAreaId
      requiredClueCount <- getPlayerCountValue (PerPlayer 2)
      unshiftMessages
        (SpendClues requiredClueCount investigatorIds
        : [ chooseOne iid [AdvanceAct aid (toSource attrs)]
          | iid <- investigatorIds
          ]
        )
      pure $ SkinGame $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      completedExtracurricularActivity <-
        elem "02041" . map unCompletedScenarioId <$> getSetList ()
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      peterClover <- PlayerCard <$> genPlayerCard "02079"
      drFrancisMorgan <- PlayerCard <$> genPlayerCard "02080"
      a <$ if completedExtracurricularActivity
        then unshiftMessages
          [ CreateStoryAssetAt peterClover "02072"
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (EncounterCardMatchByType (EnemyType, Just Abomination))
          , NextAct actId "02068"
          ]
        else unshiftMessages
          [CreateStoryAssetAt drFrancisMorgan "02076", NextAct actId "02068"]
    FoundEncounterCard _ target ec | isTarget attrs target ->
      a <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) "02072")
    _ -> SkinGame <$> runMessage msg attrs
