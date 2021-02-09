module Arkham.Types.Act.Cards.GetTheEngineRunning
  ( GetTheEngineRunning(..)
  , getTheEngineRunning
  ) where


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
