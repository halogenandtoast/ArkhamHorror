module Arkham.Types.Act.Cards.CampusSafety where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype CampusSafety = CampusSafety Attrs
  deriving newtype (Show, ToJSON, FromJSON)

campusSafety :: CampusSafety
campusSafety =
  CampusSafety $ baseAttrs "02047" "CampusSafety" (Act 3 A) Nothing

instance ActionRunner env => HasActions env CampusSafety where
  getActions i window (CampusSafety x) = getActions i window x

instance ActRunner env => RunMessage env CampusSafety where
  runMessage msg (CampusSafety attrs@Attrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide A attrs -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      alchemyLabsInPlay <- isJust
        <$> getId @(Maybe LocationId) (LocationWithTitle "Alchemy Labs")
      agendaStep <- asks $ unAgendaStep . getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      unshiftMessages
        $ [ PlaceLocationMatching (LocationWithTitle "Alchemy Labs")
          | not alchemyLabsInPlay
          ]
        <> [ CreateEnemyAtLocationMatching
               "02058"
               (LocationWithTitle "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationMatching
               "02059"
               (LocationWithTitle "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure $ CampusSafety $ attrs & sequenceL .~ Act 3 B
    _ -> CampusSafety <$> runMessage msg attrs
