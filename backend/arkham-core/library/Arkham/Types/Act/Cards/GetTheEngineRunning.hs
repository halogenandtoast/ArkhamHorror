module Arkham.Types.Act.Cards.GetTheEngineRunning
  ( GetTheEngineRunning(..)
  , getTheEngineRunning
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

newtype GetTheEngineRunning = GetTheEngineRunning ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getTheEngineRunning :: GetTheEngineRunning
getTheEngineRunning = GetTheEngineRunning
  $ baseAttrs "02166" "Get the Engine Running!" (Act 2 A) Nothing

instance ActionRunner env => HasActions env GetTheEngineRunning where
  getActions i window (GetTheEngineRunning x) = do
    mEngineCar <- getLocationIdWithTitle "Engine Car"
    case mEngineCar of
      Just engineCar -> do
        mustAdvance <- (== 0) . unClueCount <$> getCount engineCar
        if mustAdvance
          then pure [Force $ AdvanceAct (actId x) (toSource x)]
          else getActions i window x
      Nothing -> getActions i window x

instance ActRunner env => RunMessage env GetTheEngineRunning where
  runMessage msg a@(GetTheEngineRunning attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage
        (chooseOne leadInvestigatorId [AdvanceAct aid (toSource attrs)])
      pure . GetTheEngineRunning $ attrs & sequenceL .~ Act 2 B
    AdvanceAct aid _ | aid == actId && onSide B attrs ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    _ -> GetTheEngineRunning <$> runMessage msg attrs
