module Arkham.Types.Act.Cards.CampusSafety where

import Arkham.Prelude

import Arkham.Types.AgendaId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.ScenarioId
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype CampusSafety = CampusSafety ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

campusSafety :: CampusSafety
campusSafety =
  CampusSafety $ baseAttrs "02047" "CampusSafety" (Act 3 A) Nothing

instance ActionRunner env => HasActions env CampusSafety where
  getActions i window (CampusSafety x) = getActions i window x

instance ActRunner env => RunMessage env CampusSafety where
  runMessage msg (CampusSafety attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      alchemyLabsInPlay <- isJust <$> getLocationIdWithTitle "Alchemy Labs"
      agendaStep <- asks $ unAgendaStep . getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()
      theExperiment <- EncounterCard <$> genEncounterCard "02058"
      alchemicalConcoction <- PlayerCard <$> genPlayerCard "02059"

      unshiftMessages
        $ [ PlaceLocationMatching (LocationWithTitle "Alchemy Labs")
          | not alchemyLabsInPlay
          ]
        <> [ CreateEnemyAtLocationMatching
               theExperiment
               (LocationWithTitle "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               alchemicalConcoction
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B
    _ -> CampusSafety <$> runMessage msg attrs
